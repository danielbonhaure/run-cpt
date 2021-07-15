
from singleton import Singleton
from errors import ConfigError

import locale
import calendar
import sys
import os
import urllib.request
import urllib.parse
import pandas as pd
import yaml
import re
import shutil
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import matplotlib.ticker as ticker
import cdsapi

from cartopy import feature
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
from contextlib import contextmanager
from typing import Dict, List, Any
from itertools import chain
from collections import namedtuple
from datetime import date
from time import sleep


@contextmanager
def localized(new_locale):
    original_locale = '.'.join(locale.getlocale())
    if new_locale == original_locale:
        yield
    else:
        locale.setlocale(locale.LC_ALL, new_locale)
        yield
        locale.setlocale(locale.LC_ALL, original_locale)


def crange(start, stop, modulo):
    if start > stop:
        return chain(range(start, modulo+1), range(1, stop))
    else:
        return range(start, stop)


class ProgressBar:

    def __init__(self, total_count: int, status_tml: str, bar_length: int = 70):
        self.actual_count: float = 0
        self.total_count: int = total_count
        self.status_tml: str = status_tml
        self.bar_length: int = bar_length

    def __refresh__(self):
        filled_len = int(round(self.bar_length * self.actual_count / float(self.total_count)))

        percents = round(100.0 * self.actual_count / float(self.total_count), 1)
        bar = '=' * filled_len + '-' * (self.bar_length - filled_len)

        sys.stdout.write(f'[{bar}] {percents}% ...{self.status_tml}\r')
        sys.stdout.flush()

    def report_advance(self, advance_count: float):
        self.actual_count += advance_count
        self.__refresh__()

    def update_count(self, actual_count: float):
        self.actual_count = actual_count
        self.__refresh__()

    @staticmethod
    def up():
        sys.stdout.write('\x1b[1A')
        sys.stdout.flush()

    @staticmethod
    def down():
        sys.stdout.write('\n')
        sys.stdout.flush()

    @staticmethod
    def clear_line():
        sys.stdout.write("\033[K")
        sys.stdout.flush()

    def open(self):
        self.update_count(0)

    @staticmethod
    def pin_up():
        sys.stdout.write('\n')

    @staticmethod
    def close():
        sys.stdout.write('\n')


class DownloadProgressBar:
    def __init__(self, file_name, bar_length: int = 70):
        self.pbar = None
        self.fnme = file_name
        self.blen = bar_length

    def __call__(self, block_num, block_size, total_size):
        if not self.pbar:
            self.pbar = ProgressBar(total_size, f"Downloading file: {self.fnme}", self.blen)
            self.pbar.down()

        downloaded = block_num * block_size

        if downloaded > total_size:
            downloaded = total_size

        advance = downloaded - self.pbar.actual_count
        self.pbar.report_advance(advance)

        if downloaded == total_size:
            self.pbar.clear_line()
            self.pbar.up()


class SecondaryProgressBar(ProgressBar):

    def open(self):
        self.down()
        super().open()

    def close(self):
        self.clear_line()
        self.up()


class MonthsProcessor:

    with localized("en_US.UTF-8"):
        months_abbr = list(calendar.month_abbr)

    with localized("en_US.UTF-8"):
        months_names = list(calendar.month_name)

    @classmethod
    def month_abbr_to_int(cls, month: str) -> int:
        return cls.months_abbr.index(month)

    @classmethod
    def month_int_to_abbr(cls, month: int) -> str:
        return cls.months_abbr[month]

    @classmethod
    def month_name_to_int(cls, month: str) -> int:
        return cls.months_names.index(month)

    @classmethod
    def month_int_to_name(cls, month: int) -> str:
        return cls.months_names[month]

    @classmethod
    def trgt_month_in_next_year(cls, start_month: str, target_month: str) -> bool:
        return cls.month_abbr_to_int(start_month) > cls.month_abbr_to_int(target_month)

    @classmethod
    def month_abbr_to_months_range(cls, month_abbr: str) -> List[int]:
        splitted = month_abbr.split('-')
        if len(splitted) == 1:
            return [cls.month_abbr_to_int(splitted[0])]
        if len(splitted) == 2:
            return [*crange(cls.month_abbr_to_int(splitted[0]), cls.month_abbr_to_int(splitted[1])+1, 12)]

    @classmethod
    def month_abbr_to_month_num_as_str(cls, month_abbr: str) -> str:
        splitted = month_abbr.split('-')
        if len(splitted) == 1:
            return f"{cls.month_abbr_to_int(splitted[0])}"
        if len(splitted) == 2:
            return f"{cls.month_abbr_to_int(splitted[0])}-{cls.month_abbr_to_int(splitted[1])}"

    @classmethod
    def month_abbr_to_month_name(cls, month_abbr: str) -> str:
        splitted = month_abbr.split('-')
        if len(splitted) == 1:
            m1i = cls.month_abbr_to_int(splitted[0])
            return f"{cls.month_int_to_name(m1i)}"
        if len(splitted) == 2:
            m1i, m3i = cls.month_abbr_to_int(splitted[0]), cls.month_abbr_to_int(splitted[1])
            return f"{cls.month_int_to_name(m1i)}-{cls.month_int_to_name(m3i)}"

    @classmethod
    def current_month_trgts_data(cls):
        month = date.today().month
        return [dict(mons=cls.month_int_to_abbr(month), tgts=cls.month_int_to_abbr(month+1)),
                dict(mons=cls.month_int_to_abbr(month), tgts=cls.month_int_to_abbr(month+2)),
                dict(mons=cls.month_int_to_abbr(month), tgts=cls.month_int_to_abbr(month+3)),
                dict(mons=cls.month_int_to_abbr(month),
                     tgts=f'{cls.month_int_to_abbr(month+1)}-{cls.month_int_to_abbr(month+3)}')]

    @classmethod
    def current_month_fcsts_data(cls):
        year, month = date.today().year, date.today().month
        return dict(fyr=year, monf=cls.month_int_to_abbr(month), nfcsts=1)


class YearsProcessor:

    @classmethod
    def trng_period_to_years_str(cls, trng_period, trgt_season) -> str:
        years_to_str = f"{trng_period.tini}"
        if trgt_season.type == 'seasonal':
            years_to_str += f"-{trng_period.tend}"
        return years_to_str

    @classmethod
    def fcst_data_to_years_str(cls, fcst_data, trgt_season, fcst_year: int) -> str:
        trgt_years = fcst_data.trgt_years_by_month(trgt_season)
        years_to_str = f"{trgt_years.get(fcst_year).pop(0)}"
        if trgt_season.type == 'seasonal':
            years_to_str += f"-{trgt_years.get(fcst_year).pop(-1)}"
        return years_to_str

    @classmethod
    def years_for_month_range(cls, year: int, trgt_season) -> List[int]:
        years = list()
        for month in trgt_season.trgt_months_range:
            years.append(year+1 if trgt_season.start_month_int > month else year)
        return years


class FilesProcessor:

    @classmethod
    def download_file_from_url(cls, download_url: str, file_path: str, min_valid_size: int):
        #
        download_url = urllib.parse.quote(download_url, safe=':/')
        # Create progress bar to track download
        pb = DownloadProgressBar(os.path.basename(file_path))
        # Download file
        f, h = urllib.request.urlretrieve(download_url, file_path, pb)
        # Check file size
        assert os.stat(file_path).st_size != 0
        assert os.stat(file_path).st_size >= min_valid_size

    @classmethod
    def download_file_from_cdsapi(cls, file_path: str, variable: str, area: List[float], min_valid_size: int):

        # Create progress bar to track interpolation
        run_status = f'Downloading file {file_path.split("/").pop(-1)} (PID: {os.getpid()})'
        pb = SecondaryProgressBar(10, run_status)

        # Open progress bar
        pb.open()

        # Create cds api Client
        c = cdsapi.Client(quiet=False)

        # Report status
        pb.update_count(1)

        # Pin up progress bar to show cds api logs
        pb.pin_up()

        # Download netcdf
        c.retrieve(
            'reanalysis-era5-land-monthly-means',
            {
                'format': 'netcdf',
                'product_type': 'monthly_averaged_reanalysis',
                'variable': variable,
                'year': list(range(1981, date.today().year + 1)),
                'month': [
                    '01', '02', '03',
                    '04', '05', '06',
                    '07', '08', '09',
                    '10', '11', '12',
                ],
                'time': '00:00',
                'area': area,  # [nla, wlo, sla, elo]
            },
            file_path)

        # report status
        pb.update_count(9)

        # check file size
        assert os.stat(file_path).st_size != 0
        assert os.stat(file_path).st_size >= min_valid_size

        # report status
        pb.update_count(10)

        # close progress bar
        sleep(0.5)
        pb.close()


class ConfigProcessor:

    @classmethod
    def nested_dict_values(cls, d: Dict):
        for v in d.values():
            if isinstance(v, dict):
                yield from ConfigProcessor.nested_dict_values(v)
            else:
                yield v

    @classmethod
    def count_iterations(cls, d: Dict):
        return round(sum([len(v)/3 for v in ConfigProcessor.nested_dict_values(d) if isinstance(v, list)]))


class CPTFileProcessor:

    def __init__(self, longitudes: List[float], latitudes: List[float]):
        self.longitudes = longitudes
        self.latitudes = latitudes

    def rename_dataframe_columns_to_cpt_format(self, df_to_rename: pd.DataFrame) -> pd.DataFrame:
        df_output = pd.DataFrame()
        for lat, lon in zip(self.latitudes, self.longitudes):
            df_est = df_to_rename.loc[lat, lon]
            df_est.rename(lambda x: f"E({lon}_{lat})", axis='columns', inplace=True)
            # se concatena en orden inverso, para que la última columna sea la primera
            df_output = pd.concat([df_est, df_output], axis=1)
        return df_output

    def dataframe_to_cpt_format_file(self, filename: str, df_to_save: pd.DataFrame):
        # Rename columns to CPT file column format
        df_to_save = self.rename_dataframe_columns_to_cpt_format(df_to_save)

        # Como en rename_dataframe_columns_to_cpt_format se concatenaron las columnas en orden inverso,
        # las latitudes y longitudes también deben usarse en orden inverso
        reversed_lat = self.latitudes.copy()
        reversed_lat.reverse()
        reversed_lon = self.longitudes.copy()
        reversed_lon.reverse()

        # Save dataframe
        with open(filename, 'w') as f:
            f.write("Stn\t" + "\t".join(df_to_save.columns.values.tolist()) + "\n")
            f.write("Lat\t" + "\t".join(map(str, reversed_lat)) + "\n")
            f.write("Lon\t" + "\t".join(map(str, reversed_lon)) + "\n")
        df_to_save.to_csv(filename, sep='\t', mode='a', header=False, na_rep='-999')

    @classmethod
    def cpt_noaa_monthly_file_to_dataframe(cls, file_name: str):

        Info = namedtuple('Info', ['year', 'month', 'field_line', 'last_line'])

        # Open monthly file, to inspect it
        df_info = list()
        with open(file_name) as fp:
            for cnt, line in enumerate(fp):
                date_regex, nrow_regex = re.search('cpt:T=(\d+)-(\d+),', line), re.search('cpt:nrow=(\d+),', line)
                if date_regex and nrow_regex:
                    year, month = int(date_regex.group(1)), int(date_regex.group(2))
                    field_line, last_line = cnt + 1, int(nrow_regex.group(1))
                    df_info.append(Info(year, month, field_line, last_line))

        # Gen dataframe for current file accessing only rows with data
        final_df = pd.DataFrame()
        for info in df_info:
            df = pd.read_csv(file_name, sep='\t', index_col=0, skiprows=info.field_line,
                             nrows=info.last_line, na_values='-999')
            df.insert(0, 'month', info.month)
            df.insert(0, 'year', info.year)
            df.set_index(['year', 'month'], append=True, inplace=True)
            final_df = pd.concat([final_df, df])

        # Return generated dataframe
        return final_df

    @classmethod
    def cpt_file_is_a_locally_created_one(cls, file_name: str) -> bool:
        """Check if file exists and it's a locally created file"""
        if os.path.exists(file_name):
            with open(file_name, 'r') as fp:
                if 'locally_created_file' in fp.readline():
                    return True
        return False

    @classmethod
    def dataframe_to_cpt_noaa_seasonal_hindcast_file(cls, file_name: str, file_data: dict, df: pd.DataFrame,
                                                     debug: bool = False):
        # Set correct file_name according to debug status
        file_name = file_name.replace('.txt', '_debug.txt') if debug else file_name

        # Order df by year
        df = df.sort_index(level=['year', 0], ascending=[True, False])

        # Backup file if there was previously downloaded instead of locally created
        if not cls.cpt_file_is_a_locally_created_one(file_name) and os.path.exists(file_name):
            _ = shutil.copy(file_name, file_name.replace('.txt', '_downloaded_from_noaa.txt'))

        # Create file, open it and add header (2 firsts lines)
        with open(file_name, "w") as fp:
            fp.write('xmlns:cpt=http://iri.columbia.edu/CPT/v10/locally_created_file\n')
            fp.write('cpt:nfields=1\n')

        # create progress bar to track grid generation
        run_status = f'Generating file {file_name} (PID: {os.getpid()})'
        cant_years = file_data['training_original_last_year'] - file_data['training_original_first_year'] + 1
        pb = SecondaryProgressBar(cant_years, run_status)

        # open progress bar
        pb.open()

        # Iter df by year and save data to the exit file
        for year in range(file_data['training_original_first_year'], file_data['training_original_last_year']+1):

            # Define target years
            first_target_month_year = year+1 if file_data["first_target_month_in_next_year"] else year
            last_target_month_year = year+1 if file_data["last_target_month_in_next_year"] else year

            # Define cpt inputs
            cpt_field = 'precip' if file_data['seasonal_file_variable'] == 'precip' else '2m Air Temperature'
            cpt_t = f'{first_target_month_year}-{file_data["first_target_month"]:02d}/'
            cpt_t += f'{last_target_month_year}-' if first_target_month_year != last_target_month_year else ''
            cpt_t += f'{file_data["last_target_month"]:02d}'
            cpt_s = f'{year}-{file_data["forecast_init_month"]:02d}-01'
            cpt_units = 'millimeters per day' if file_data['seasonal_file_variable'] == 'precip' else 'Degrees Celcius'

            # Define line that describe year
            y_line = f'cpt:field={cpt_field}, cpt:T={cpt_t}, cpt:S={cpt_s}, cpt:nrow=181, cpt:ncol=360, ' \
                     f'cpt:row=Y, cpt:col=X, cpt:units={cpt_units}, cpt:missing=-999.00000000000000{" "*15}'

            # Add line that describe the year to save
            with open(file_name, "a") as fp:
                fp.write(f'{y_line}\n')

            # Filter data to save
            df_y = df.loc[df.index.get_level_values('year') == year].droplevel(level='year')

            # Add df_y to the exit file
            df_y.sort_index(ascending=False).to_csv(file_name, sep='\t', na_rep='-999.000000', mode='a')

            # Report advance
            pb.report_advance(1)

        # close progress bar
        pb.close()

    @classmethod
    def dataframe_to_cpt_noaa_seasonal_forecast_file(cls, file_name: str, file_data: dict, df: pd.DataFrame,
                                                     debug: bool = False):
        # Set correct file_name according to debug status
        file_name = file_name.replace('.txt', '_debug.txt') if debug else file_name

        # Create file, open it and add header (2 firsts lines)
        with open(file_name, "w") as fp:
            fp.write('xmlns:cpt=http://iri.columbia.edu/CPT/v10/\n')
            fp.write('cpt:nfields=1\n')

        # Get forecast year
        year = file_data["forecast_year"]

        # Define target years
        first_target_month_year = year+1 if file_data["first_target_month_in_next_year"] else year
        last_target_month_year = year+1 if file_data["last_target_month_in_next_year"] else year

        # Define cpt inputs
        cpt_field = 'precip' if file_data['seasonal_file_variable'] == 'precip' else '2m Air Temperature'
        cpt_t = f'{first_target_month_year}-{file_data["first_target_month"]:02d}/'
        cpt_t += f'{last_target_month_year}-' if first_target_month_year != last_target_month_year else ''
        cpt_t += f'{file_data["last_target_month"]:02d}'
        cpt_s = f'{year}-{file_data["forecast_init_month"]:02d}-01'
        cpt_units = 'millimeters per day' if file_data['seasonal_file_variable'] == 'precip' else 'Degrees Celcius'

        # Define line that describe year
        y_line = f'cpt:field={cpt_field}, cpt:T={cpt_t}, cpt:S={cpt_s}, cpt:nrow=181, cpt:ncol=360, ' \
                 f'cpt:row=Y, cpt:col=X, cpt:units={cpt_units}, cpt:missing=-999.00000000000000{" "*15}'

        # Add line that describe the year to save
        with open(file_name, "a") as fp:
            fp.write(f'{y_line}\n')

        # Add df_y to the exit file
        df.sort_index(ascending=False).to_csv(file_name, sep='\t', na_rep='-999.000000', mode='a')


@Singleton
class ConfigFile:

    def __init__(self):
        self._file_name: str = 'config.yaml'
        self.cpt_config: dict = self.__load_config()
        self.__create_plot_yaml_file()

    def __load_config(self) -> dict:
        if not os.path.exists(self._file_name):
            raise ConfigError(f"Configuration file (i.e. {self._file_name}) not found!")
        with open(self._file_name, 'r') as f:
            return yaml.safe_load(f)

    def __create_plot_yaml_file(self):
        with open(self.cpt_config.get('files').get('plot_yaml'), 'w') as fp_plot_yaml:
            fp_plot_yaml.write('\nfolders:\n')
            fp_plot_yaml.write(f'  observed_data: {self.cpt_config.get("folders").get("predictands")}\n')
            fp_plot_yaml.write(f'  generated_data: {self.cpt_config.get("folders").get("output")}\n')
            fp_plot_yaml.write(f'  shapefiles: {self.cpt_config.get("folders").get("raw_data").get("shapefiles")}\n')
            fp_plot_yaml.write(f'  output: {self.cpt_config.get("folders").get("output")}\n')
            fp_plot_yaml.write('\nfiles:\n')

    @property
    def file_name(self):
        return self._file_name

    @file_name.setter
    def file_name(self, value):
        self._file_name = value
        self.cpt_config = self.__load_config()

    def get(self, key, default=None) -> Any:
        return self.cpt_config.get(key, default)


@Singleton
class UpdateControl:

    def __init__(self):
        self._config_file: str = 'config.yaml'
        self._updated_files: set = set()
        self._update_config: dict = dict()
        self.__load_global_update_config()

    def __load_global_update_config(self):
        if not os.path.exists(self._config_file):
            raise ConfigError(f"Configuration file (i.e. {self._config_file}) not found!")
        with open(self._config_file, 'r') as f:
            self._update_config = yaml.safe_load(f).get('update', {})

    @property
    def config_file(self):
        return self._config_file

    @config_file.setter
    def config_file(self, value):
        self._config_file = value
        self.__load_global_update_config()

    def must_be_updated(self, k1: str, k2: str, file_abs_path: str) -> bool:
        # k1 can be "predictors" or "predictands"
        # k2 can be "raw_data", ... , "cpt_input_data"
        if self.file_already_updated(file_abs_path):
            return False
        return self._update_config.get(k1, {}).get(k2, False)

    def report_updated_file(self, file_abs_path: str):
        self._updated_files.add(file_abs_path)

    def file_already_updated(self, file_abs_path: str) -> bool:
        return file_abs_path in self._updated_files


def plt_predictors_and_predictands_domains(use_topo: bool = True):
    """A simple plot function for the geographical domain of predictors and predictands

    PARAMETERS
    ----------
        use_topo: Put a background image on for nice sea rendering.
    """

    config = ConfigFile.Instance()

    """ loni: western longitude
        lone: eastern longitude
        lati: southern latitude
        late: northern latitude """
    loni1 = config.get('spatial_domain').get('predictor').get('wlo')
    lone1 = config.get('spatial_domain').get('predictor').get('elo')
    lati1 = config.get('spatial_domain').get('predictor').get('sla')
    late1 = config.get('spatial_domain').get('predictor').get('nla')

    """ loni: western longitude
        lone: eastern longitude
        lati: southern latitude
        late: northern latitude """
    loni2 = config.get('spatial_domain').get('predictand').get('wlo')
    lone2 = config.get('spatial_domain').get('predictand').get('elo')
    lati2 = config.get('spatial_domain').get('predictand').get('sla')
    late2 = config.get('spatial_domain').get('predictand').get('nla')

    # Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
    states_provinces = feature.NaturalEarthFeature(
        category='cultural',
        name='admin_0_countries',
        scale='10m',
        facecolor='none')

    fig = plt.subplots(figsize=(15, 15), subplot_kw=dict(projection=ccrs.PlateCarree()))
    loni = [loni1, loni2]  # loni: western longitude
    lati = [lati1, lati2]  # lati: southern latitude
    lone = [lone1, lone2]  # lone: eastern longitude
    late = [late1, late2]  # late: northern latitude
    title = ['Predictor', 'Predictand']

    for i in range(2):

        ax = plt.subplot(1, 2, i+1, projection=ccrs.PlateCarree())
        ax.set_extent([loni[i], lone[i], lati[i], late[i]], ccrs.PlateCarree())

        # Put a background image on for nice sea rendering.
        if use_topo:
            ax.stock_img()

        ax.add_feature(feature.LAND)
        ax.add_feature(feature.COASTLINE)
        ax.add_feature(feature.OCEAN)
        ax.set_title(title[i]+" domain")
        pl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                          linewidth=2, color='gray', alpha=0.5, linestyle='--')
        pl.xlabels_top = False
        pl.ylabels_left = False
        pl.xformatter = LONGITUDE_FORMATTER
        pl.yformatter = LATITUDE_FORMATTER
        pl.xlocator = ticker.MaxNLocator(4)
        pl.ylocator = ticker.MaxNLocator(4)
        ax.add_feature(states_provinces, edgecolor='gray')
    plt.show()


def pltdomain(nla: float, sla: float, wlo: float, elo: float,
              title: str = 'Geographical Domain', use_topo: bool = True):
    """A simple plot function for a geographical domain

    PARAMETERS
    ----------
        nla: Northernmost latitude.
        sla: Southernmost latitude.
        wlo: Westernmost longitude.
        elo: Easternmost longitude.
        title: Plot title (default = Geographical Domain).
        use_topo: Put a background image on for nice sea rendering.
    """

    # Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
    states_provinces = feature.NaturalEarthFeature(
        category='cultural',
        name='admin_0_countries',
        scale='10m',
        facecolor='none')

    ax = plt.subplot(projection=ccrs.PlateCarree())
    ax.set_extent([wlo, elo, sla, nla], ccrs.PlateCarree())

    # Put a background image on for nice sea rendering.
    if use_topo:
        ax.stock_img()

    ax.add_feature(feature.LAND)
    ax.add_feature(feature.COASTLINE)
    ax.add_feature(feature.OCEAN)
    ax.set_title(f"{title} domain")
    pl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                      linewidth=2, color='gray', alpha=0.5, linestyle='--')
    pl.xlabels_top = False
    pl.ylabels_left = False
    pl.xformatter = LONGITUDE_FORMATTER
    pl.yformatter = LATITUDE_FORMATTER
    pl.xlocator = ticker.MaxNLocator(4)
    pl.ylocator = ticker.MaxNLocator(4)
    ax.add_feature(states_provinces, edgecolor='gray')

    plt.show()
