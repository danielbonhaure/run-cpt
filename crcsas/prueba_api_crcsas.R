rm(list = ls()); gc()

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

datos_estaciones <- ConsumirServicioJSON(url = paste0(base.url, "/estaciones"),
                                         usuario = usuario.default, clave = clave.default) 


#
# PRCP
#

estaciones_prcp <- ConsumirServicioJSON(
  url = paste0(base.url, "/registros_mensuales/estaciones_completas/prcp/1991/2020"),
  usuario = usuario.default, clave = clave.default)
#
datos_prcp <- purrr::map_dfr(
  .x = estaciones_prcp$omm_id,
  .f = function(omm_id) {
    ConsumirServicioJSON(url = paste0(base.url, "/registros_mensuales/", omm_id, "/prcp/sum/1991/2020"),
                         usuario = usuario.default, clave = clave.default)
  }
) %>% dplyr::select(omm_id, anho, mes, prcp = valor)
#
datos_estaciones_prcp <- datos_estaciones %>% 
  dplyr::filter(omm_id %in% estaciones_prcp$omm_id)
#
g_prcp <- ggplot2::ggplot(data = datos_estaciones_prcp) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = longitud, 
      y = latitud,
      colour = "red",
      size = 3)) +
  ggplot2::geom_sf(
    data =  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
    fill = "white",
    alpha = 0.05) +
  ggplot2::coord_sf(
    xlim = c(-78+1, -34.1-1),
    ylim = c(-60+3, -10+1),
    expand = FALSE) +
  ggplot2::labs(fill = "") +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::ggtitle(
    "Estaciones completas", subtitle = "Periodo 1991-2020") +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(
      color = gray(0.8), linetype = "dashed", size = 0.5),
    panel.background = ggplot2::element_rect(
      colour = "gray", size = 2, fill = "white"),
    legend.key.height = ggplot2::unit(1, 'cm'),
    legend.position = "none") 
#
gg_prcp <- ggiraph::girafe(
  ggobj = g_prcp + 
    ggiraph::geom_point_interactive(
      data = datos_estaciones_prcp,
      mapping = ggplot2::aes(
        x = longitud, y = latitud,
        tooltip = omm_id),
      alpha = 0.01,
      show.legend = FALSE),
  width_svg = 12, 
  height_svg = 9)


#
# TMED
#

estaciones_tmax <- ConsumirServicioJSON(
  url = paste0(base.url, "/registros_mensuales/estaciones_completas/tmax/1991/2020"),
  usuario = usuario.default, clave = clave.default)
datos_tmax <- purrr::map_dfr(
  .x = estaciones_tmax$omm_id,
  .f = function(omm_id) {
    ConsumirServicioJSON(url = paste0(base.url, "/registros_mensuales/", omm_id, "/tmax/prom/1991/2020"),
                         usuario = usuario.default, clave = clave.default)
  }
) %>% dplyr::select(omm_id, anho, mes, tmax = valor)
#
estaciones_tmin <- ConsumirServicioJSON(
  url = paste0(base.url, "/registros_mensuales/estaciones_completas/tmin/1991/2020"),
  usuario = usuario.default, clave = clave.default)
datos_tmin <- purrr::map_dfr(
  .x = estaciones_tmin$omm_id,
  .f = function(omm_id) {
    ConsumirServicioJSON(url = paste0(base.url, "/registros_mensuales/", omm_id, "/tmin/prom/1991/2020"),
                         usuario = usuario.default, clave = clave.default)
  }
) %>% dplyr::select(omm_id, anho, mes, tmin = valor)
#
datos_tmed <- dplyr::inner_join(datos_tmax, datos_tmin, 
                                by = c("omm_id", "anho", "mes")) %>%
  dplyr::mutate(tmed = (tmax + tmin) / 2) %>%
  dplyr::select(omm_id, anho, mes, tmed)
#
estaciones_tmed <- 
  dplyr::inner_join(estaciones_tmax, estaciones_tmin, by="omm_id")
#
datos_estaciones_tmed <- datos_estaciones %>% 
  dplyr::filter(omm_id %in% estaciones_tmed$omm_id) %>%
  dplyr::filter(latitud >= -60)
#
g_tmed <- ggplot2::ggplot(data = datos_estaciones_tmed) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = longitud, 
      y = latitud,
      colour = "red",
      size = 3)) +
  ggplot2::geom_sf(
    data =  rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"),
    fill = "white",
    alpha = 0.05) +
  ggplot2::coord_sf(
    xlim = c(-78+1, -34.1-1),
    ylim = c(-60+3, -10+1),
    expand = FALSE) +
  ggplot2::labs(fill = "") +
  ggplot2::xlab("") +
  ggplot2::ylab("") +
  ggplot2::ggtitle(
    "Estaciones completas", subtitle = "Periodo 1991-2020") +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(
      color = gray(0.8), linetype = "dashed", size = 0.5),
    panel.background = ggplot2::element_rect(
      colour = "gray", size = 2, fill = "white"),
    legend.key.height = ggplot2::unit(1, 'cm'),
    legend.position = "none") 
#
gg_tmed <- ggiraph::girafe(
  ggobj = g_tmed + 
    ggiraph::geom_point_interactive(
      data = datos_estaciones_tmed,
      mapping = ggplot2::aes(
        x = longitud, y = latitud,
        tooltip = omm_id),
      alpha = 0.01,
      show.legend = FALSE),
  width_svg = 12, 
  height_svg = 9)
