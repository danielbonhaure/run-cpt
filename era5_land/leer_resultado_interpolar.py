
# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/era5_land'])
# os.chdir('era5_land')

import xarray as xr
import pandas as pd

from latitudes import latitudes
from longitudes import longitudes


def renombrar_columnas_formato_cpt(df_original: pd.DataFrame) -> pd.DataFrame:
    data = pd.DataFrame()
    for lat, lon in zip(latitudes, longitudes):
        df_est = df_original.loc[lat, lon]
        df_est.rename(lambda x: f"E({lon}_{lat})", axis='columns', inplace=True)
        data = pd.concat([data, df_est], axis=1)
    return data


def guardar_archivo_formato_cpt(filename: str, df: pd.DataFrame):
    with open(filename, 'w') as f:
        f.write("Stn\t" + "\t".join(df.columns.values.tolist()) + "\n")
        f.write("Lat\t" + "\t".join(map(str, latitudes)) + "\n")
        f.write("Lon\t" + "\t".join(map(str, longitudes)) + "\n")
    df.to_csv(filename, sep='\t', mode='a', header=False)


# Leer netcdf con datos interpolados
ds_kelvin = xr.open_dataset('resultado_interpolar_xr.nc')
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


# Crear archivo de control
guardar_archivo_formato_cpt(
    filename='resultado_final_completo.tsv',
    df=renombrar_columnas_formato_cpt(df_celsius)
)


# Se debe reordenar para poder seleccionar mes/es
df_celsius = df_celsius.reorder_levels(['latitude', 'longitude', 'month', 'year'])
df_celsius = df_celsius.sort_index()


# Definir mes objetivo
month = 5
# Obtener datos del mes objetivo
df_month = df_celsius.loc[:, :, month, :]
# Crear archivo
guardar_archivo_formato_cpt(
    filename=f'resultado_final_mes_{month}.tsv',
    df=renombrar_columnas_formato_cpt(df_month)
)


# Definir mes objetivo
first_month, last_month = 2, 4
# Obtener datos del mes objetivo
df_season = df_celsius.loc[:, :, first_month:last_month, :]
# Agrupar datos y calcular promedio
df_season = df_season.groupby(['latitude', 'longitude', 'year']).mean().round(1)
# Crear archivo
guardar_archivo_formato_cpt(
    filename=f'resultado_final_meses_{first_month}-{last_month}.tsv',
    df=renombrar_columnas_formato_cpt(df_season)
)

