
# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/chirps'])
# os.chdir('chirps')


import json
import logging
import numpy as np
import pandas as pd

import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages

from rpy2.robjects.vectors import FloatVector, StrVector
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter
from rpy2.rinterface_lib.callbacks import logger as rpy2_logger


from latitudes import latitudes
from longitudes import longitudes

# Remove warnings messages
rpy2_logger.setLevel(logging.ERROR)   # will display errors, but not warnings

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

# import R's package to be used
dplyr = rpackages.importr('dplyr')
tibble = rpackages.importr('tibble')
raster = rpackages.importr('raster')

# interpolar
s = raster.stack("chirps_recortado.nc", varname="precip")
p = tibble.tibble(longitude=FloatVector(longitudes), latitude=FloatVector(latitudes))
e = raster.extract(s, p, method="bilinear", df=True)
f = dplyr.inner_join(tibble.rowid_to_column(p, "ID"), e, by='ID')

# convertir df de R a df de pandas
with localconverter(robjects.default_converter + pandas2ri.converter):
    pd_from_r_df = robjects.conversion.rpy2py(f)

# pivotar df
df = pd_from_r_df.melt(id_vars=["ID", "longitude", "latitude"], var_name="time", value_name="precip")
df['time'] = pd.to_datetime(df['time'], format='X%Y.%m.%d', errors='ignore')
df = df.drop(columns=['ID']).set_index(['time', 'latitude', 'longitude']).sort_index()

# generar xarray
xx = df.to_xarray().fillna(np.nan)
# ds.precip[0, :, :].plot.imshow()

# Identificar puntos no interpolados
# El problema con los puntos no interpolados es porque chirps
# sí tiene datos suficientes para interpolar esos puntos, o sea,
# la diferencia al interpolar con python y R se da solo con era5!
nan_lons, nan_lats = list(), list()
for lon, lat in zip(longitudes, latitudes):
    if np.all(np.isnan(xx.precip.sel(longitude=lon, latitude=lat).values)):
        nan_lons.append(lon)
        nan_lats.append(lat)
# open output file for writing
with open('nan_lons_R.txt', 'w') as filehandle:
    json.dump(nan_lons, filehandle)
with open('nan_lats_R.txt', 'w') as filehandle:
    json.dump(nan_lats, filehandle)
# open output file for reading
with open('nan_lons_R.txt', 'r') as filehandle:
    nan_lons = json.load(filehandle)
with open('nan_lats_R.txt', 'r') as filehandle:
    nan_lats = json.load(filehandle)
# plot(nan_lons, nan_lats, 'bo', color='red')

# guardar netcf con datos interpolados
xx.to_netcdf('resultado_interpolar_R.nc')
