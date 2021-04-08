
from helpers import SecondaryProgressBar, ConfigFile, UpdateControl

import logging
import numpy as np
import pandas as pd
import geopandas as gpd
import os

import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages

from typing import Union, List
from shapely.geometry import Point

from rpy2.robjects.vectors import FloatVector, StrVector
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter
from rpy2.rinterface_lib.callbacks import logger as rpy2_logger

# remove warnings messages
rpy2_logger.setLevel(logging.ERROR)  # will display errors, but not warnings


class CrcSasGrid:

    def __init__(self):
        # load variables
        self.install_r_packages()
        self.longitudes: List[float] = []
        self.latitudes: List[float] = []
        self.file_name: str = "points_in_land.shp"
        self.__set_lats_lons()

    @property
    def abs_path(self):
        config = ConfigFile.Instance()
        folder = config.get('folders').get('crc_sas_grid')
        return os.path.join(folder, self.file_name) if self.file_name else ""

    def __set_lats_lons(self):
        config = ConfigFile.Instance()
        update_ctrl = UpdateControl.Instance()

        if not os.path.exists(self.abs_path) or \
                update_ctrl.must_be_updated('crc_sas_grid', 'points_in_land', self.abs_path):

            # Generate  longitude an latitude values
            lats = np.arange(config.get('spatial_domain').get('crc_sas_grid').get('sla'),
                             config.get('spatial_domain').get('crc_sas_grid').get('nla') + 0.5,
                             0.5)
            lons = np.arange(config.get('spatial_domain').get('crc_sas_grid').get('wlo'),
                             config.get('spatial_domain').get('crc_sas_grid').get('elo') + 0.5,
                             0.5)

            # Make a grid of latitude-longitude values
            xx, yy = np.meshgrid(lons, lats)

            # Now convert these points to geo-data
            pts = gpd.GeoSeries([Point(x, y) for x, y in zip(xx.flatten(), yy.flatten())])

            # Get shape-file indicating land
            shp_folder = config.get('folders').get('raw_data').get('shapefiles')
            boros = gpd.GeoDataFrame.from_file(os.path.join(shp_folder, 'CRC_SAS.shp'))

            # Identify points in land
            in_map: List[bool] = []
            # -- create progress bar to track in land points identification
            pb_i = SecondaryProgressBar(len(pts), f'Identifying points in land (PID: {os.getpid()})')
            # -- open progress bar
            pb_i.open()
            # -- iterate over points
            for p in pts:
                in_map.append(True) if any([p.within(geom) for geom in boros.geometry]) else in_map.append(False)
                pb_i.report_advance(1)
            # -- close progress bar
            pb_i.close()

            # Select only the points in land
            pts = gpd.GeoSeries([val for pos, val in enumerate(pts) if in_map[pos]])

            # Save new file
            pts.to_file(self.abs_path)

        else:
            # Read existing file
            pts = gpd.GeoDataFrame.from_file(self.abs_path)

        # Get coordinates in map
        self.longitudes = list(pts.geometry.x)  # list(xx.flatten())
        self.latitudes = list(pts.geometry.y)  # list(np.flip(yy.flatten()))

    def get_longitudes(self):
        return self.longitudes

    def get_latitudes(self):
        return self.latitudes

    @classmethod
    def install_r_packages(cls):
        # import R's utility package
        utils = rpackages.importr('utils')

        # R package names
        packnames = ('ncdf4', 'raster', 'dplyr', 'tibble')

        # Selectively install what needs to be install.
        # We are fancy, just because we can.
        names_to_install = [x for x in packnames if not rpackages.isinstalled(x)]
        if len(names_to_install) > 0:
            # select a mirror for R packages
            utils.chooseCRANmirror(ind=1)  # select the first mirror in the list
            # install packages
            utils.install_packages(StrVector(names_to_install))

    def interpolate_raw_data(self, input_file: str, variable: str, convert_kelvin_to_celsius: bool = False,
                             return_df: bool = False, output_file: str = None, show_progress_bar: bool = True
                             ) -> Union[str, pd.DataFrame]:

        # create progress bar to track interpolation
        run_status = f'Interpolating {input_file.split("/").pop(-1)} (PID: {os.getpid()})'
        pb = SecondaryProgressBar(10, run_status) if show_progress_bar else None

        # open progress bar
        pb.open() if show_progress_bar else None

        # define output file name, if there is not specified by parameter
        if not output_file:
            output_file = input_file.replace('.nc', '_interpolated.nc')

        # import R's package to be used
        dplyr = rpackages.importr('dplyr')
        tibble = rpackages.importr('tibble')
        raster = rpackages.importr('raster')

        # report status
        pb.update_count(0.5) if show_progress_bar else None

        # open netcdf as raster stack
        s = raster.stack(input_file, varname=variable)

        # report status
        pb.update_count(0.5) if show_progress_bar else None

        # create points to be interpolated
        p = tibble.tibble(longitude=FloatVector(self.longitudes), latitude=FloatVector(self.latitudes))

        # report status
        pb.update_count(3) if show_progress_bar else None

        # interpolate
        e = raster.extract(s, p, method="bilinear", df=True)

        # report status
        pb.update_count(6) if show_progress_bar else None

        # add an ID to the interpolated data
        f = dplyr.inner_join(tibble.rowid_to_column(p, "ID"), e, by='ID')

        # report status
        pb.update_count(7) if show_progress_bar else None

        # convert the R dataframe to a pandas dataframe
        with localconverter(robjects.default_converter + pandas2ri.converter):
            pd_from_r_df = robjects.conversion.rpy2py(f)

        # report status
        pb.update_count(8) if show_progress_bar else None

        # pivot pandas dataframe
        df = pd_from_r_df.melt(id_vars=["ID", "longitude", "latitude"], var_name="time", value_name=variable)
        df['time'] = pd.to_datetime(df['time'], format='X%Y.%m.%d', errors='ignore')
        df = df.drop(columns=['ID']).set_index(['time', 'latitude', 'longitude']).sort_index()

        # report status
        pb.update_count(9) if show_progress_bar else None

        # convert pandas dataframe to xarray object
        xx = df.to_xarray().fillna(np.nan)

        # If needed, convert measure unit
        if convert_kelvin_to_celsius:
            # set variable unit to kelvin
            xx[variable].attrs['units'] = 'kelvin'
            # convert data from kelvin to celsius
            xx = xx - 273.15
            # set variable unit to celsius
            xx[variable].attrs['units'] = 'celsius'
            # round converted data
            xx = xx[variable].round(1)

        # save netcdf with interpolated data
        xx.to_netcdf(output_file)

        # report status
        pb.update_count(10) if show_progress_bar else None

        # define value to be returned
        value_to_return = output_file if not return_df else xx.to_dataframe()

        # close progress bar
        pb.close() if show_progress_bar else None

        # return
        return value_to_return
