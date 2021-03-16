
# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/chirps'])
# os.chdir('chirps')


import yaml
import xarray as xr


with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

coords = config.get('spatial_domain').get('predictand')


# Leer netcdf con datos descargados
ds = xr.open_dataset('chirps-v2.0.monthly.nc')
mask = ((ds.coords["latitude"] >= coords['sla']) & (ds.coords["latitude"] <= coords['nla']) &
        (ds.coords["longitude"] >= coords['wlo']) & (ds.coords["longitude"] <= coords['elo']))
ds = ds.where(mask, drop=True)

# Guardar netcdf
ds.to_netcdf('chirps_recortado.nc')
