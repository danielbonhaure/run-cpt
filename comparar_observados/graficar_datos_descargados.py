
import xarray as xr


archivo_temporal = "input/raw_data/era5_land/prcp_era5-land.nc"

ds = xr.open_dataset(archivo_temporal)
ds.prcp[0, :, :].plot.imshow()

ds['prcp'] = ds.prcp.assign_attrs(units='mm')
ds['prcp'].values = ds.prcp.values * 1000
ds['prcp'].values = ds.prcp.values.round(1)
ds.prcp[0, :, :].plot.imshow()


archivo_temporal = "input/raw_data/era5_land/t2m_era5-land.nc"

ds = xr.open_dataset(archivo_temporal)
ds.t2m[0, :, :].plot.imshow()

ds['t2m'] = ds.t2m.assign_attrs(units='°C')
ds['t2m'].values = ds.t2m.values - 273.15
ds['t2m'].values = ds.t2m.values.round(1)
ds.t2m[0, :, :].plot.imshow()


archivo_temporal = "obs_marisol/prcp.nc"

ds = xr.open_dataset(archivo_temporal)
ds.prcp[0, :, :].plot.imshow()
