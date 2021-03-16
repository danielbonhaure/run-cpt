
# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/era5_land'])
# os.chdir('era5_land')

import numpy as np
import xarray as xr

from matplotlib.pyplot import plot

from latitudes import latitudes
from longitudes import longitudes


# Get coordinates
lats = list(set(latitudes))
lons = list(set(longitudes))

# Sort coordinates
lats.sort()
lons.sort()


# A partir de los puntos definidos en los archivos (latitudes.py y longitudes.py)
# se crea un netcdf en el que se indica con 0 los puntos del netcdf que no están
# en los archivos y con 1 los que sí están. Los puntos marcado con 1, son además,
# puntos que se encuentran sobre la superficie continental y no sobre el océano!
data = np.zeros((len(lats), len(lons)), dtype=int)
ds = xr.Dataset(
    data_vars={'land': (['latitude', 'longitude'],  data)},
    coords={'longitude': (['longitude'], lons), 'latitude': (['latitude'], lats)}
)
for lon, lat in zip(longitudes, latitudes):
    lon_index, lat_index = lons.index(lon), lats.index(lat)
    ds.land[lat_index, lon_index] = 1
ds.to_netcdf('crcsas_land.nc')
ds.close()

# Es el netcdf creado a partir de los puntos con los que vamos
# a trabajar (archivos: latitudes.py y longitudes.py).
ds = xr.open_dataset('crcsas_land.nc')
ds.land[:, :].plot.imshow()

# Este es el netcdf descargado desde copernicus y contiene
# los datos del dataset Era5-Land!!
dd = xr.open_dataset('download.nc')
dd.t2m[0, :, :].plot.imshow()

# Este es el netcdf creado luego de interpolar los datos del
# dataset Era5-Land descarados de copernicus.
dd = xr.open_dataset('resultado_interpolar_cf.nc')
dd.t2m[0, :, :].plot.imshow()

# Esto simplemente grafica los puntos en los archivos (latitudes.py y longitudes.py)
plot(longitudes, latitudes, 'bo')
