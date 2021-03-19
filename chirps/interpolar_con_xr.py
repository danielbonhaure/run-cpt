
# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/chirps'])
# os.chdir('chirps')


import json
import xarray as xr
import numpy as np
import pandas as pd

from matplotlib.pyplot import plot

from typing import Tuple

from latitudes import latitudes
from longitudes import longitudes


def get_unique_coords(lons_coords: list, lats_coords: list) -> Tuple[list, list]:
    # Get unique coordinates
    lons = list(set(lons_coords))
    lats = list(set(lats_coords))
    # Sort coordinates
    lons.sort()
    lats.sort()
    # Return results
    return lons, lats


# Leer netcdf con datos descargados
ds = xr.open_dataset('chirps_recortado.nc')
# ds.precip[0, :, :].plot.imshow()

# Interpolar datos usando el metodo bilinear raster::extract
lons, lats = get_unique_coords(longitudes, latitudes)
xx = ds.interp(longitude=lons, latitude=lats, method="linear")
# xx.precip[0, :, :].plot.imshow()

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
with open('nan_lons_xr.txt', 'w') as filehandle:
    json.dump(nan_lons, filehandle)
with open('nan_lats_xr.txt', 'w') as filehandle:
    json.dump(nan_lats, filehandle)
# open output file for reading
with open('nan_lons_xr.txt', 'r') as filehandle:
    nan_lons = json.load(filehandle)
with open('nan_lats_xr.txt', 'r') as filehandle:
    nan_lats = json.load(filehandle)
# plot(nan_lons, nan_lats, 'bo', color='red')

# Guardar netcdf
xx.to_netcdf('resultado_interpolar_xr.nc')

