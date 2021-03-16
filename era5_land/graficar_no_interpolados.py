
# OBS: copiar y pegar en la consola python

# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/era5_land'])
# os.chdir('era5_land')

import xarray as xr
import numpy as np

from matplotlib.pyplot import plot

from latitudes import latitudes
from longitudes import longitudes


# Get coordinates
lats = list(set(latitudes))
lons = list(set(longitudes))

# Sort coordinates
lats.sort()
lons.sort()


# Leer netcdf con datos descargados
ds = xr.open_dataset('download.nc')

# Interpolar datos usando el metodo bilinear raster::extract
ds_kelvin = ds.interp(longitude=lons, latitude=lats, method="linear")
ds_kelvin.t2m.attrs['units'] = 'kelvin'

# Convertir los datos de kelvin a celsius
ds_celsius = ds_kelvin - 273.15
ds_celsius.t2m.attrs['units'] = 'celsius'
ds_celsius = ds_celsius.t2m.round(1)

# Convertir datos a un dataframe de pandas
df_celsius = ds_celsius.to_dataframe()
df_celsius['year'] = df_celsius.index.get_level_values('time').year
df_celsius['month'] = df_celsius.index.get_level_values('time').month
df_celsius.set_index('year', append=True, inplace=True)
df_celsius.set_index('month', append=True, inplace=True)
df_celsius = df_celsius.droplevel('time')
df_celsius = df_celsius.sort_index()

# Cerrar netcdf con datos interpolados
ds_kelvin.close()
ds_celsius.close()


# Identificar puntos no interpolados
nan_lats, nan_lons = list(), list()
for lat, lon in zip(latitudes, longitudes):
    if np.all(np.isnan(df_celsius.loc[lat, lon].values)):
        nan_lats.append(lat)
        nan_lons.append(lon)
# Graficar puntos no interpolados
ds.t2m[0, :, :].plot.imshow()
plot(longitudes, latitudes, 'bo')
plot(nan_lons, nan_lats, 'bo', color='red')

# Análisis de uno de los puntos no interpolados
ds_kelvin.sel(longitude=-37.25, latitude=-11.25).t2m.values
ds.sel(longitude=-37.25, latitude=-11.25, method='ffill').coords['latitude']

ds.sel(longitude=-37.25, latitude=-11.25, method='bfill')
ds.sel(longitude=-37.25, latitude=-11.25, method='nearest')
ds.sel(longitude=-37.2, latitude=-11.3).t2m.values
ds.sel(longitude=-37.3, latitude=-11.2).t2m.values
ds.sel(longitude=-37.2, latitude=-11.2).t2m.values
ds.sel(longitude=-37.3, latitude=-11.3).t2m.values
# Ver punto no interpolado y cercanos
ds.t2m[0, :, :].plot.imshow()
plot(-37.25, -11.25, 'bo', color='blue')
plot(-37.3, -11.2, 'bo', color='red')
plot(-37.2, -11.3, 'bo', color='orange')
plot(-37.2, -11.2, 'bo', color='black')
plot(-37.3, -11.3, 'bo', color='black')

