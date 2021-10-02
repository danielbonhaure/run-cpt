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


base.url        <- '192.168.100.224:8080'
usuario.default <- '***REMOVED***' 
clave.default   <- '***REMOVED***'


estaciones <- ConsumirServicioJSON(
  url = paste0(base.url, "/registros_mensuales/estaciones_completas/1991/2020"),
  usuario = usuario.default, clave = clave.default)

datos_prcp <- purrr::map_dfr(
  .x = estaciones$omm_id,
  .f = function(omm_id) {
    ConsumirServicioJSON(url = paste0(base.url, "/registros_mensuales/", omm_id, "/prcp/sum/1991/2020"),
                         usuario = usuario.default, clave = clave.default)
  }
) %>% dplyr::select(omm_id, anho, mes, prcp = valor)

datos_tmax <- purrr::map_dfr(
  .x = estaciones$omm_id,
  .f = function(omm_id) {
    ConsumirServicioJSON(url = paste0(base.url, "/registros_mensuales/", omm_id, "/tmax/prom/1991/2020"),
                         usuario = usuario.default, clave = clave.default)
  }
) %>% dplyr::select(omm_id, anho, mes, tmax = valor)

datos_tmin <- purrr::map_dfr(
  .x = estaciones$omm_id,
  .f = function(omm_id) {
    ConsumirServicioJSON(url = paste0(base.url, "/registros_mensuales/", omm_id, "/tmin/prom/1991/2020"),
                         usuario = usuario.default, clave = clave.default)
  }
) %>% dplyr::select(omm_id, anho, mes, tmin = valor)

datos_tmed <- dplyr::inner_join(datos_tmax, datos_tmin, 
                                by = c("omm_id", "anho", "mes")) %>%
  dplyr::mutate(tmed = (tmax + tmin) / 2) %>%
  dplyr::select(omm_id, anho, mes, tmed)

