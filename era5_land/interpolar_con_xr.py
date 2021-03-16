
# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/era5_land'])
# os.chdir('era5_land')


import json
import warnings
import xarray as xr
import numpy as np
import pandas as pd

from matplotlib.pyplot import plot
from typing import Tuple
from scipy.spatial import distance
from scipy.interpolate import interp2d
from scipy.interpolate import Rbf
from time import time

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


start = time()
# Leer netcdf con datos descargados
ds = xr.open_dataset('download.nc')
# ds.t2m[0, :, :].plot.imshow()
print(f"Tiempo lectura netcdf: {time()-start} segundos")

start = time()
# Interpolar datos usando el metodo bilinear raster::extract
lons, lats = get_unique_coords(longitudes, latitudes)
xx = ds.interp(longitude=lons, latitude=lats, method="linear")
# xx.t2m[0, :, :].plot.imshow()
print(f"Tiempo interpolación: {time()-start} segundos")

start = time()
# Identificar puntos no interpolados
# El problema con los puntos no interpolados es porque chirps
# sí tiene datos suficientes para interpolar esos puntos, o sea,
# la diferencia al interpolar con python y R se da solo con era5!
nan_lons, nan_lats = list(), list()
for lon, lat in zip(longitudes, latitudes):
    if np.all(np.isnan(xx.t2m.sel(longitude=lon, latitude=lat).values)):
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
print(f"Tiempo obtener coordenadas de NAs: {time()-start} segundos")


start = time()
# Como python retorna más NAs que raster::extract, se vuelven a interpolar
# las coordenadas que quedaron con NA. La idea es aplicar el mismo algoritmo
# que raster::extract. Es decir, aplicar el algoritmo "bilineas interpolation"
# así como lo aplica el paquete raster::extract de R.
contador = 0
total_na = len(nan_lons)
print(f"\rInterpolando NAs ({contador} de {total_na})", end="")
for lon, lat in zip(nan_lons, nan_lats):
    start_ciclo = time()
    contador += 1
    # print(lon, lat)
    # lon, lat = -69.75, -33.25
    # lon, lat = -37.25, -11.25
    # lon, lat = -64.25, -54.75

    # El orden de las celdas en la matriz de 2x2 a ser utilizado es el que
    # se utiliza en el paquete raster::extract de R, y es el siguiente:
    # p2 p4
    # p1 p3
    p2 = dict(
        longitude=round(float(ds.t2m.sel(longitude=lon, latitude=lat, method='ffill').longitude), 1),
        latitude=round(float(ds.t2m.sel(longitude=lon, latitude=lat, method='ffill').latitude), 1)
    )
    p3 = dict(
        longitude=round(float(ds.t2m.sel(longitude=lon, latitude=lat, method='bfill').longitude), 1),
        latitude=round(float(ds.t2m.sel(longitude=lon, latitude=lat, method='bfill').latitude), 1)
    )
    # Una vez identificados los puntos p2 y p3, es posible calcular los puntos p1 y p4
    p1 = dict(
        longitude=p2['longitude'],
        latitude=p3['latitude']
    )
    p4 = dict(
        longitude=p3['longitude'],
        latitude=p2['latitude']
    )

    for year in ds.time.values:
        # print(f"\t\t{year}")
        # year = ds.time.values[0]

        #
        # Esto es lo que hace raster::extract cuando hay NAs
        #

        # el orden en R -> # v2 v4
        # el orden en R -> # v1 v3
        v1 = float(ds.t2m.sel(longitude=p1['longitude'], latitude=p1['latitude'], time=year))
        v2 = float(ds.t2m.sel(longitude=p2['longitude'], latitude=p2['latitude'], time=year))
        v3 = float(ds.t2m.sel(longitude=p3['longitude'], latitude=p3['latitude'], time=year))
        v4 = float(ds.t2m.sel(longitude=p4['longitude'], latitude=p4['latitude'], time=year))

        # Con 4 NAs, el algoritmo no se aplica
        if np.count_nonzero(np.isnan([v1, v2, v3, v4])) == 4:
            continue

        # Con 3 NAs, se hace lo siguiente
        if np.count_nonzero(np.isnan([v1, v2, v3, v4])) == 3:
            continue

        # Con 1 o 2 NAs, se hace lo que sigue
        # 1ero remplaza valores por columna
        if np.isnan(v1):
            v1 = v2
        if np.isnan(v2):
            v2 = v1
        if np.isnan(v3):
            v3 = v4
        if np.isnan(v4):
            v4 = v3

        # despues de esto calcula el promedio sin NAs
        with warnings.catch_warnings():
            warnings.simplefilter("ignore", category=RuntimeWarning)
            vX = np.nanmean([v1, v2, v3, v4])

        # despues, para los NAs que aún queden, los reemplazo por el promedio anterior
        # esto pasa cuando los dos valores de una columna son NA
        if np.isnan(v1):
            v1 = vX
        if np.isnan(v2):
            v2 = vX
        if np.isnan(v3):
            v3 = vX
        if np.isnan(v4):
            v4 = vX

        interp_lons = [p2['longitude'], p3['longitude']]
        interp_lats = [p3['latitude'], p2['latitude']]
        f = interp2d(interp_lons, interp_lats, [[v1, v2], [v3, v4]])

        xx.t2m.loc[dict(longitude=lon, latitude=lat, time=year)] = float(f(lon, lat))
    print(f"\rInterpolando NAs ({contador} de {total_na} - {time()-start_ciclo} segundos)", end="")
print(f"\rTiempo interpolación faltantes: {time()-start} segundos")


start = time()
# Identificar puntos no interpolados
# El problema con los puntos no interpolados es porque chirps
# sí tiene datos suficientes para interpolar esos puntos, o sea,
# la diferencia al interpolar con python y R se da solo con era5!
nan_lons, nan_lats = list(), list()
for lon, lat in zip(longitudes, latitudes):
    if np.all(np.isnan(xx.t2m.sel(longitude=lon, latitude=lat).values)):
        nan_lons.append(lon)
        nan_lats.append(lat)
# open output file for writing
with open('nan_lons_xr_rellenado.txt', 'w') as filehandle:
    json.dump(nan_lons, filehandle)
with open('nan_lats_xr_rellenado.txt', 'w') as filehandle:
    json.dump(nan_lats, filehandle)
# open output file for reading
with open('nan_lons_xr_rellenado.txt', 'r') as filehandle:
    nan_lons = json.load(filehandle)
with open('nan_lats_xr_rellenado.txt', 'r') as filehandle:
    nan_lats = json.load(filehandle)
# plot(nan_lons, nan_lats, 'bo', color='red')
print(f"Tiempo obtener coordenadas de NAs (luego del rellenado): {time()-start} segundos")


start = time()
# Guardar netcdf
xx.to_netcdf('resultado_interpolar_xr.nc')
print(f"Tiempo guardado netcdf: {time()-start} segundos")

