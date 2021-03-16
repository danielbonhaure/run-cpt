
import cf

from latitudes import latitudes
from longitudes import longitudes


# Get coordinates
lats = list(set(latitudes))
lons = list(set(longitudes))

# Sort coordinates
lats.sort()
lons.sort()


# ver: https://ncas-cms.github.io/cf-python/tutorial.html#regridding-methods
lat = cf.DimensionCoordinate(data=cf.Data(lats, 'degrees_north'))
lon = cf.DimensionCoordinate(data=cf.Data(lons, 'degrees_east'))

# cf.read devuelve una lista con un solo elemento,
# es ese único elemento el que hay que interpolar
f = cf.read('download.nc').pop()

# para usar regrid hay que instalar pyESMF (python -m pip install pyESMF)
# para install pyESMF hay que instalar cmake (sudo apt install cmake)
# para que funcione el paquete pyESMF hay que instalar libgfortran-7-dev
c = f.regrids({'latitude': lat, 'longitude': lon}, method='linear', use_dst_mask=True)

cf.write(c, 'resultado_interpolar_cf.nc')
