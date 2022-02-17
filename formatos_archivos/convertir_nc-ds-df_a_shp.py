
import numpy as np
import xarray as xr

from geopandas import GeoDataFrame
from shapely.geometry import Point

with xr.open_dataset("input/raw_data/chirps/prcp_chirps_daily_interpolated.nc") as ds:
    df = ds.sel(time="2000").to_dataframe()

df = df.reset_index()
del df['time']

geometry = [Point(xy) for xy in zip(df.longitude, df.latitude)]
df = df.drop(['longitude', 'latitude'], axis=1)
gdf = GeoDataFrame(df, crs="EPSG:4326", geometry=geometry)

gdf.to_file("puntos_interp_chirps.shp")

gdf = gdf.loc[~np.isnan(gdf.prcp)]

gdf.to_file("puntos_interp_chirps_non_nan.shp")
