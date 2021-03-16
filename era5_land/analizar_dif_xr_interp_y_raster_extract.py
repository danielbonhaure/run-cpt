
# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/era5_land'])
# os.chdir('era5_land')

import xarray as xr
import numpy as np

from matplotlib.pyplot import plot
from scipy.interpolate import interp2d

from latitudes import latitudes
from longitudes import longitudes


# Leer netcdf con datos descargados
ds = xr.open_dataset('download.nc')

ds.t2m[0, :, :].plot.imshow()
# punto a ser interpolado
plot(-69.75, -33.25, 'bo', color='green')
# puntos adyacentes
plot(-69.8, -33.2, 'bo', color='red')
plot(-69.7, -33.2, 'bo', color='red')
plot(-69.8, -33.3, 'bo', color='red')
plot(-69.7, -33.3, 'bo', color='red')  # punto al que agrego NA
# fila de arriba
plot(-69.9, -33.1, 'bo', color='yellow')
plot(-69.8, -33.1, 'bo', color='yellow')
plot(-69.7, -33.1, 'bo', color='yellow')
plot(-69.6, -33.1, 'bo', color='yellow')
# fila del punto utilizado
plot(-69.9, -33.2, 'bo', color='orange')  # punto utilizado por raster::extract bilinear
plot(-69.6, -33.2, 'bo', color='yellow')
# fila corta sin punto utlizado
plot(-69.9, -33.3, 'bo', color='yellow')
plot(-69.6, -33.3, 'bo', color='yellow')
# fila de abajo
plot(-69.9, -33.4, 'bo', color='yellow')
plot(-69.8, -33.4, 'bo', color='yellow')
plot(-69.7, -33.4, 'bo', color='yellow')
plot(-69.6, -33.4, 'bo', color='yellow')

# Punto inferior derecho con respecto al punto a interpolar
ds.t2m.sel(longitude=-69.7, latitude=-33.3, time='1981-01-01')

# modificar valores de puntos más cercanos al punto a interpolar
ds.t2m.loc[dict(longitude=-69.8, latitude=-33.2, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.7, latitude=-33.2, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.8, latitude=-33.3, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.7, latitude=-33.3, time='1981-01-01')] = np.nan

# Punto a ser utilizado cuando hay 1 NA
ds.t2m.sel(longitude=-69.9, latitude=-33.2, time='1981-01-01')

# Punto a ser utilizado cuando hay 2 NAs
ds.t2m.sel(longitude=-69.8, latitude=-33.1, time='1981-01-01')

# modificar valores de puntos más lejanos al punto a interpolar
# fila de arriba
ds.t2m.loc[dict(longitude=-69.9, latitude=-33.1, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.8, latitude=-33.1, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.7, latitude=-33.1, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.6, latitude=-33.1, time='1981-01-01')] = np.nan
# fila del punto utilizado
ds.t2m.loc[dict(longitude=-69.9, latitude=-33.2, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.6, latitude=-33.2, time='1981-01-01')] = np.nan
# fila corta sin punto utlizado
ds.t2m.loc[dict(longitude=-69.9, latitude=-33.3, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.6, latitude=-33.3, time='1981-01-01')] = np.nan
# fila de abajo
ds.t2m.loc[dict(longitude=-69.9, latitude=-33.4, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.8, latitude=-33.4, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.7, latitude=-33.4, time='1981-01-01')] = np.nan
ds.t2m.loc[dict(longitude=-69.6, latitude=-33.4, time='1981-01-01')] = np.nan


# Esto es lo que hace raster::extract cuando hay NAs

# el orden en R -> # p2 p4
# el orden en R -> # p1 p3
p1 = float(ds.t2m.sel(longitude=-69.8, latitude=-33.3, time='1981-01-01'))
p2 = float(ds.t2m.sel(longitude=-69.8, latitude=-33.2, time='1981-01-01'))
p3 = np.nan  # float(ds.t2m.sel(longitude=-69.7, latitude=-33.3, time='1981-01-01'))
p4 = float(ds.t2m.sel(longitude=-69.7, latitude=-33.2, time='1981-01-01'))

# 1ero remplaza valores por columna
if np.isnan(p1):
    p1 = p2
if np.isnan(p2):
    p2 = p1
if np.isnan(p3):
    p3 = p4
if np.isnan(p4):
    p4 = p3

# despues de esto calcula el promedio sin NAs
pr = np.nanmean([p1, p2, p3, p4])

# despues, para los NAs que aún queden, los reemplazo por el promedio anterior
# esto pasa cuando los dos valores de una columna son NA
if np.isnan(p1):
    p1 = pr
if np.isnan(p2):
    p2 = pr
if np.isnan(p3):
    p3 = pr
if np.isnan(p4):
    p4 = pr

f = interp2d([-69.8, -69.7], [-33.3, -33.2], [[p1, p2], [p3, p4]])

r_con_nan = f(-69.75, -33.25)  # 272.27866552685657

# Resultado de interpolar con python
xx = xr.open_dataset('resultado_interpolar_xr.nc')
float(xx.t2m.sel(longitude=-36.75, latitude=-10.25, time='1981-01-01'))
float(xx.t2m.sel(longitude=-69.75, latitude=-33.25, time='1981-01-01'))
float(xx.t2m.sel(longitude=-74.75, latitude=-52.75, time='1981-01-01'))

# Identificar puntos no interpolados
xx_nan_lats, xx_nan_lons = list(), list()
for lat, lon in zip(latitudes, longitudes):
    if np.all(np.isnan(xx.t2m.loc[:, lat, lon].values)):
        xx_nan_lats.append(lat)
        xx_nan_lons.append(lon)
plot(xx_nan_lons, xx_nan_lats, 'bo', color='red')


# Resultado de interpolar con R (sin NA)
yy = xr.open_dataset('resultado_interpolar_R.nc')
float(yy.t2m.sel(longitude=-36.75, latitude=-10.25, time='1981-01-01'))
float(yy.t2m.sel(longitude=-69.75, latitude=-33.25, time='1981-01-01'))
float(yy.t2m.sel(longitude=-74.75, latitude=-52.75, time='1981-01-01'))

# Identificar puntos no interpolados
yy_nan_lats, yy_nan_lons = list(), list()
for lat, lon in zip(latitudes, longitudes):
    if np.all(np.isnan(yy.t2m.loc[:, lat, lon].values)):
        yy_nan_lats.append(lat)
        yy_nan_lons.append(lon)
plot(yy_nan_lons, yy_nan_lats, 'bo', color='violet')


# Resultado de interpolar con R (con NA)
zz = xr.open_dataset('resultado_interpolar_R_nan.nc')
float(zz.t2m.sel(longitude=-36.75, latitude=-10.25, time='1981-01-01'))
float(zz.t2m.sel(longitude=-69.75, latitude=-33.25, time='1981-01-01'))
float(zz.t2m.sel(longitude=-74.75, latitude=-52.75, time='1981-01-01'))

# Identificar puntos no interpolados
zz_nan_lats, zz_nan_lons = list(), list()
for lat, lon in zip(latitudes, longitudes):
    if np.all(np.isnan(zz.t2m.loc[:, lat, lon].values)):
        zz_nan_lats.append(lat)
        zz_nan_lons.append(lon)
plot(yy_nan_lons, yy_nan_lats, 'bo', color='violet')


# CONCLUSIONES:
#
# Analizando los puntos no interpolados por R y python
# En un primer momento me pareció que el paquete raster::extract utiliza los 4 puntos más cercanos, sin considerar la
# distancia de estos al punto a interpolar. Sin embargo, modificando los valores de las celdas que consideré eran
# utilizadas, me di cuenta de que, en realidad, el paquete raster::extract utiliza, al igual que el paquete
# xarray.interp de python, utiliza solo los 4 puntos adyacentes!!. La única diferencia es que el paquete raster::extract
# de R permite eliminar los NAs del cálculo y funciona con hasta 2 adycacentes con NA, es decir retonar NA cuando solo
# una celda adyacente tenga datos, o cuando ninguna celda adyacente tenga datos. Mientras que el paquete xarray.interp
# no permite eliminar los NAs del cálculo, por lo tanto, retorna NA ya cuando uno de los adyacentes es NA.
