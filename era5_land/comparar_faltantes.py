
# import sys
# import os
# sys.path.extend(['/home/dbonhaure/PycharmProjects/PyCPT/era5_land'])
# os.chdir('era5_land')

import json
import xarray as xr

from matplotlib.pyplot import plot

# Leer netcdf con datos descargados
ds = xr.open_dataset('download.nc')
ds.t2m[0, :, :].plot.imshow()

with open('nan_lons_R.txt', 'r') as filehandle:
    r_nan_lons = json.load(filehandle)
with open('nan_lats_R.txt', 'r') as filehandle:
    r_nan_lats = json.load(filehandle)
plot(r_nan_lons, r_nan_lats, 'bo', color='red')

with open('nan_lons_xr.txt', 'r') as filehandle:
    py_nan_lons = json.load(filehandle)
with open('nan_lats_xr.txt', 'r') as filehandle:
    py_nan_lats = json.load(filehandle)
plot(py_nan_lons, py_nan_lats, 'bo', color='violet')
