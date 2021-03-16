
# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/era5_land'])
# os.chdir('era5_land')


import json
import xarray as xr
import numpy as np
import pandas as pd

from matplotlib.pyplot import plot
from typing import Tuple
from scipy.spatial import distance
from scipy.interpolate import interp2d
from scipy.interpolate import Rbf

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
ds = xr.open_dataset('download.nc')
# ds.t2m[0, :, :].plot.imshow()

# Interpolar datos usando el metodo bilinear raster::extract
lons, lats = get_unique_coords(longitudes, latitudes)
xx = ds.interp(longitude=lons, latitude=lats, method="linear")
# xx.t2m[0, :, :].plot.imshow()

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


# Como python retorna más NAs que raster::extract, se vuelven a interpolar
# las coordenadas que quedaron con NA. La idea es aplicar el mismo algoritmo
# que raster::extract. Es decir, aplicar el algoritmo "bilineas interpolation"
# así como lo aplica el paquete raster::extract de R.
for lon, lat in zip(nan_lons, nan_lats):
    print(lon, lat)
    # lon, lat = -69.75, -33.25
    # lon, lat = -37.25, -11.25

    # El orden de las celdas en la matriz de 2x2 a ser utilizado:
    # p1 p2
    # p3 p4
    p1 = dict(
        longitude=round(float(ds.t2m.sel(longitude=lon, latitude=lat, method='ffill').longitude), 1),
        latitude=round(float(ds.t2m.sel(longitude=lon, latitude=lat, method='ffill').latitude), 1)
    )
    p4 = dict(
        longitude=round(float(ds.t2m.sel(longitude=lon, latitude=lat, method='bfill').longitude), 1),
        latitude=round(float(ds.t2m.sel(longitude=lon, latitude=lat, method='bfill').latitude), 1)
    )
    mask = ((ds.coords["latitude"] >= p4['latitude']) & (ds.coords["latitude"] <= p1['latitude']) &
            (ds.coords["longitude"] >= p1['longitude']) & (ds.coords["longitude"] <= p4['longitude']))
    points = ds.where(mask, drop=True)
    X = np.array([(lat, lon)])  # https://stackoverflow.com/a/48610147
    coords = np.array([(la, lo) for la in points.latitude.values for lo in points.longitude.values])
    coords_matrix = coords.reshape(len(points.latitude), len(points.longitude), -1)
    distancias = xr.DataArray(
        data=distance.cdist(X, coords, 'euclidean').reshape(len(points.latitude), len(points.longitude)),
        coords=[points.latitude.values, points.longitude.values], dims=["latitude", "longitude"])
    df_dist = distancias.to_dataset(name='dist')\
        .to_dataframe().reset_index()
    df_dist = df_dist[['longitude', 'latitude', 'dist']]
    df_dist.longitude = df_dist.longitude.round(1)
    df_dist.latitude = df_dist.latitude.round(1)
    df_dist.sort_values(
        by=['dist', 'latitude', 'longitude'],
        ascending=[True, False, True],
        inplace=True)
    df_dist.reset_index(inplace=True, drop=True)

    for year in points.time.values:
        # year = points.time.values[0]
        print(f"\t\t{year}")
        df_valores = points.t2m.sel(time=year)\
            .to_dataframe().reset_index()
        df_valores = df_valores[['longitude', 'latitude', 't2m']]
        df_valores.longitude = df_valores.longitude.round(1)
        df_valores.latitude = df_valores.latitude.round(1)
        df_valores = pd.merge(df_dist, df_valores, on=['longitude', 'latitude'], how='inner')
        df_valores.sort_values(
            by=['dist', 'latitude', 'longitude'],
            ascending=[True, False, True],
            inplace=True)
        df_valores.dropna(inplace=True)
        df_valores.reset_index(inplace=True, drop=True)

        if len(df_valores.index) >= 4:
            rbfi = Rbf(df_valores[0:4].longitude, df_valores[0:4].latitude, df_valores[0:4].t2m, function='linear')
            xx.t2m.loc[dict(longitude=lon, latitude=lat, time=year)] = float(rbfi(lon, lat))


# Guardar netcdf
xx.to_netcdf('resultado_interpolar_xr.nc')

