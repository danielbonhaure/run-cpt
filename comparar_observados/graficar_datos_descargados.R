
rm(list = ls()); gc()

library(dplyr, quietly = T)


#
# Punto a ser analizado
#

# Función para acceder a un servicio web definido por una URL utilizando el método GET.
# Devuelve la respuesta como texto plano.
ConsumirServicioGET <- function(url, usuario, clave) {
  req  <- httr::GET(url = url, 
                    config = httr::authenticate(user = usuario, 
                                                password = clave))
  return (httr::content(req, as = "text"))
}
# Función para acceder a un servicio web definido por una URL utilizando un usuario y clave.
# Asumiendo que la respuesta es un string JSON, hace la conversión del mismo a Data Frame.
ConsumirServicioJSON <- function(url, usuario, clave) {
  respuesta <- ConsumirServicioGET(url, usuario, clave)
  return (jsonlite::fromJSON(respuesta))
}
# Credenciales de acceso
base.url        <- 'https://api.crc-sas.org/ws-api'
usuario.default <- '********'
clave.default   <- '********'
# Búsqueda de estaciones a través de servicio web
estaciones <- ConsumirServicioJSON(url = paste0(base.url, "/estaciones"),
                                   usuario = usuario.default, clave = clave.default)
estacion_pehuajo <- estaciones %>% dplyr::filter(omm_id == 87544)
coord.objetivo <- tibble::tibble(
  longitude=estacion_pehuajo$longitud, latitude=estacion_pehuajo$latitud)
# Limpiar ambiente
rm(estaciones, estacion_pehuajo); gc()


#
# ERA5-LAND editado con python
#

# a. Definir el archivo
archivo.temporal <- "input/raw_data/era5_land/prcp_era5-land.nc"
# b. Leer archivo en formato largo (x, y, fecha, prcp).
# prcp.era5land.crcsas <- raster::stack(archivo.temporal, varname = "prcp") %>%
#   raster::rasterToPoints() %>% dplyr::as_tibble() %>%
#   tidyr::pivot_longer(cols = !c('x', 'y'), names_to = 'fecha_string', values_to = 'prcp') %>%
#   dplyr::mutate(fecha = as.Date(fecha_string, format = "X%Y.%m.%d")) %>%
#   dplyr::select(x, y, fecha, prcp) %>%
#   # redondear a un solo decimal
#   dplyr::mutate(prcp = round(prcp, 1))
# d. Extraer un solo punto
prcp.era5land.coord <- raster::stack(archivo.temporal, varname="tp") %>%
  raster::extract(coord.objetivo, method="bilinear", df=TRUE) %>%
  tidyr::pivot_longer(!ID, names_to = 'fecha_string', values_to = 'prcp') %>%
  dplyr::mutate(fecha = as.Date(fecha_string, format = "X%Y.%m.%d")) %>%
  dplyr::select(fecha, prcp) %>%
  # redondear a un solo decimal
  dplyr::mutate(prcp = round(prcp, 1)) %>%
  # preparar join
  dplyr::mutate(year = lubridate::year(fecha), month = lubridate::month(fecha)) %>%
  dplyr::select(year, month, prcp.era5land = prcp)
# e. Limpiar ambiente
rm(archivo.temporal); gc()


#
# ERA5-LAND original
#

# a. Definir el archivo
archivo.temporal <- "era5_land/prcp.nc"
# b. Leer archivo en formato largo (x, y, fecha, prcp).
# prcp.era5land.crcsas <- raster::stack(archivo.temporal, varname = "prcp") %>%
#   raster::rasterToPoints() %>% dplyr::as_tibble() %>%
#   tidyr::pivot_longer(cols = !c('x', 'y'), names_to = 'fecha_string', values_to = 'prcp') %>%
#   dplyr::mutate(fecha = as.Date(fecha_string, format = "X%Y.%m.%d")) %>%
#   dplyr::select(x, y, fecha, prcp) %>%
#   # # Pasar de m a mm
#   # dplyr::mutate(prcp = prcp * 1000) %>%
#   # # Pasar de promedio a cantidad mensual
#   # dplyr::mutate(prcp = prcp * lubridate::days_in_month(fecha)) %>%
#   # redondear a un solo decimal
#   dplyr::mutate(prcp = round(prcp, 1))
# d. Extraer un solo punto
prcp.era5land.coord <- raster::stack(archivo.temporal, varname="tp") %>%
  raster::extract(coord.objetivo, method="bilinear", df=TRUE) %>%
  tidyr::pivot_longer(!ID, names_to = 'fecha_string', values_to = 'prcp') %>%
  dplyr::mutate(fecha = as.Date(fecha_string, format = "X%Y.%m.%d")) %>%
  dplyr::select(fecha, prcp) %>%
  # Pasar de m a mm
  dplyr::mutate(prcp = prcp * 1000) %>%
  # Pasar de promedio a cantidad mensual
  dplyr::mutate(prcp = prcp * lubridate::days_in_month(fecha)) %>%
  # redondear a un solo decimal
  dplyr::mutate(prcp = round(prcp, 1)) %>%
  # preparar join
  dplyr::mutate(year = lubridate::year(fecha), month = lubridate::month(fecha)) %>%
  dplyr::select(year, month, prcp.era5land = prcp)
# e. Limpiar ambiente
rm(archivo.temporal); gc()


#
# CRC-SAS
#

# a. Definir el archivo
archivo.temporal <- "input/raw_data/crcsas/prcp_crcsas.csv"
# b. Leer archivo
# prcp.crcsas <- read.csv(archivo.temporal, sep = ';')
# c. Extraer un solo punto
prcp.crcsas.coord <- read.csv(archivo.temporal, sep = ';') %>%
  dplyr::filter(longitude == coord.objetivo$longitude,
                latitude == coord.objetivo$latitude) %>%
  dplyr::mutate(fecha = as.Date(time)) %>%
  dplyr::select(fecha, prcp) %>%
  # preparar join
  dplyr::mutate(year = lubridate::year(fecha), month = lubridate::month(fecha)) %>%
  dplyr::select(year, month, prcp.crcsas = prcp)
# d. Limpiar ambiente
rm(archivo.temporal); gc()


#
# CHIRPS
#

# a. Definir el archivo
archivo.temporal <- "input/raw_data/chirps/prcp_chirps_daily.nc"
# b. Leer archivo en formato largo (x, y, fecha, prcp).
# prcp.chirps <- raster::stack(archivo.temporal, varname="prcp") %>%
#   raster::rasterToPoints() %>% dplyr::as_tibble() %>%
#   tidyr::pivot_longer(cols = !c('x', 'y'), names_to = 'fecha_string', values_to = 'prcp') %>%
#   dplyr::mutate(fecha = as.Date(fecha_string, format = "X%Y.%m.%d")) %>%
#   dplyr::select(x, y, fecha, prcp) %>%
#   # redondear a un solo decimal
#   dplyr::mutate(prcp = round(prcp, 1))
# d. Extraer un solo punto
prcp.chirps.coord <- raster::stack(archivo.temporal, varname="prcp") %>%
  raster::extract(coord.objetivo, method="bilinear", df=TRUE) %>%
  tidyr::pivot_longer(!ID, names_to = 'fecha_string', values_to = 'prcp') %>%
  dplyr::mutate(fecha = as.Date(fecha_string, format = "X%Y.%m.%d")) %>%
  dplyr::select(fecha, prcp) %>%
  # redondear a un solo decimal
  dplyr::mutate(prcp = round(prcp, 1)) %>%
  # preparar join
  dplyr::mutate(year = lubridate::year(fecha), month = lubridate::month(fecha)) %>%
  dplyr::select(year, month, prcp.chirps = prcp)
# e. Limpiar ambiente
rm(archivo.temporal); gc()


#
# NOAA MARISOL
#

# a. Definir el archivo
archivo.temporal <- "obs_marisol/prcp.nc"
# b. Leer archivo en formato largo (x, y, fecha, prcp).
# prcp.era5.ms.crcsas <- raster::stack(archivo.temporal, varname="prate") %>%
#   raster::rasterToPoints() %>% dplyr::as_tibble() %>%
#   tidyr::pivot_longer(cols = !c('x', 'y'), names_to = 'fecha_string', values_to = 'prcp') %>%
#   dplyr::mutate(
#     n_months = as.numeric(stringr::str_replace_all(fecha_string, 'X', '')),
#     fecha = lubridate::date('1960-01-01') + lubridate::dmonths(n_months)) %>%
#   dplyr::select(x, y, fecha, prcp) %>%
#   # pasar de longitud de 0 a 360 a longitud de -180 a 180
#   dplyr::mutate(x = x - 180) %>%
#   # redondear a un solo decimal
#   dplyr::mutate(prcp = round(prcp, 1))
#
# crcsas <- sf::st_read("./input/raw_data/shapefiles/CRC_SAS.shp")
# crcsas <- sf::st_set_crs(crcsas, 4326)
# puntos <- sf::st_as_sf(prcp.noaa.ms %>% dplyr::select(x, y) %>% dplyr::distinct(), coords = c('x', 'y'))
# puntos <- sf::st_set_crs(puntos, 4326)
# library(sf, quietly = T)
# plot(sf::st_geometry(crcsas))
# plot(sf::st_geometry(puntos), add=T)
#
# d. Extraer un solo punto
prcp.noaa.ms.coord <- raster::stack(archivo.temporal, varname="prate") %>%
  # pasar de longitud de 0 a 360 a longitud de -180 a 180
  raster::extract(coord.objetivo %>% dplyr::mutate(longitude = longitude + 180), 
                  method="bilinear", df=TRUE) %>%
  tidyr::pivot_longer(cols = !ID, names_to = 'fecha_string', values_to = 'prcp') %>%
  dplyr::mutate(
    n_months = as.numeric(stringr::str_replace_all(fecha_string, 'X', '')),
    fecha = lubridate::date('1960-01-01') + lubridate::dmonths(n_months)) %>%
  dplyr::select(fecha, prcp) %>%
  # redondear a un solo decimal
  dplyr::mutate(prcp = round(prcp, 1)) %>%
  # preparar join
  dplyr::mutate(year = lubridate::year(fecha), month = lubridate::month(fecha)) %>%
  dplyr::select(year, month, prcp.noaa.ms = prcp)
# e. Limpiar ambiente
rm(archivo.temporal); gc()


#
# ERA5-LAND datos diarios
#

# a. Definir el archivo
archivo.temporal <- "era5_land/prcp_por_hora.nc"
# b. Extraer un solo punto
prcp.era5land.hr.coord <- raster::stack(archivo.temporal, varname="tp") %>%
  raster::extract(coord.objetivo, method="bilinear", df=TRUE) %>%
  tidyr::pivot_longer(cols = !ID, names_to = 'fecha_string', values_to = 'prcp') %>%
  dplyr::mutate(fecha = as.POSIXct(fecha_string, format="X%Y.%m.%d.%H.%M.%S")) %>%
  # pasar de m a mm
  dplyr::mutate(prcp = prcp * 1000) %>%
  dplyr::select(fecha, prcp) %>%
  # sumar toda la preicpitación en un mes año
  dplyr::mutate(year = lubridate::year(fecha), month = lubridate::month(fecha), 
                day = lubridate::day(fecha), hour = lubridate::hour(fecha)) %>%
  # selecciono solo la observación de ma media noche que es la  
  # acumulación máxima del día anterior.
  dplyr::filter(hour == 0) %>%
  # resto un día
  dplyr::mutate(n_fecha = as.Date(paste(year, month, day , sep="-")) - 1) %>%
  dplyr::mutate(n_year = lubridate::year(n_fecha), n_month = lubridate::month(n_fecha)) %>%
  dplyr::filter(n_year >= min(year)) %>%
  # redondear a un solo decimal
  dplyr::mutate(prcp = round(prcp, 1)) %>%
  dplyr::group_by(n_year, n_month) %>%
  dplyr::summarise(prcp = sum(prcp)) %>%
  # redondear a un solo decimal
  dplyr::mutate(prcp = round(prcp, 1)) %>%
  # preparar join
  dplyr::select(year = n_year, month = n_month, prcp.era5land.hr = prcp)
# c. Limpiar ambiente
rm(archivo.temporal); gc()


#
# UNIR TODO
#

prcp.coord <- prcp.era5land.coord %>%
  dplyr::left_join(prcp.era5land.hr.coord, by = c('year', 'month')) %>%
  dplyr::left_join(prcp.crcsas.coord, by = c('year', 'month')) %>%
  dplyr::left_join(prcp.chirps.coord, by = c('year', 'month')) %>%
  dplyr::left_join(prcp.noaa.ms.coord, by = c('year', 'month'))


#
# GRAFICAR
#

prcp.coord.g <- prcp.coord %>%
  dplyr::filter(year >= 2010, year <= 2014) %>%
  dplyr::mutate(fecha = glue::glue('{year}-{month}-01')) %>%
  dplyr::select(fecha, era5land = prcp.era5land, era5land.hr = prcp.era5land.hr, 
                crcsas = prcp.crcsas, chirps = prcp.chirps, noaa.ms = prcp.noaa.ms) %>%
  tidyr::pivot_longer(cols = !fecha, names_to = 'fuente', values_to = 'prcp')
#
ggplot2::ggplot(data = prcp.coord.g) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      x = as.Date(fecha), y = prcp, group = fuente, col = fuente)) +
  ggplot2::labs(
    x = 'Fecha', y = 'Precipitación (mm)', 
    title = 'Era5-Land vs CHIRPS vs CRC-SAS en Pehuajó (87544)',
    subtitle = 'Precipitación en milímetros (mm)', 
    col = 'Precipitación') +
  ggplot2::scale_x_date(
    date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_color_manual(
    values = c("era5land" = "blue", "era5land.hr" = "purple", 
               "crcsas" = "red", "chirps" = "black", "noaa.ms" = "green"),
    labels = c("era5land" = "Era5-Land", "era5land.hr" = "Era5-Land (hr)", 
               "crcsas" = "CRC-SAS", "chirps" = "CHIRPS", "noaa.ms" = "NOAA (MS)")) +
  ggplot2::theme_bw() + 
  ggplot2::theme(
    legend.position = 'bottom', 
    plot.title = ggplot2::element_text(hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5)
  )
