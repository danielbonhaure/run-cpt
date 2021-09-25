
################################################################################
# Graficar datos (probabilidades separadas - escala continua - op 1)  #####
generar_graficos_prob_sep_continuos_op_1 <- function() {
  
  # Se agrega el PATH al nombre del archivo, pero aún no se define la 
  # extensión, es decir, aún no se define el tipo de archivo.
  fig_file_name <- paste0(config$folders$plots, "continuous_scales/", fig_file_name)
  
  # Definir bounding box de los gráficos (tanto para leaflet como para ggplot2)
  bbox <- list(
    left = config$spatial_domain$wlo, bottom = config$spatial_domain$sla,
    right = config$spatial_domain$elo, top = config$spatial_domain$nla)
  
  ##############################################################################
  ## GRAFICOS CON LEAFLET (https://rstudio.github.io/leaflet/raster.html) ####
  
  # Con leaflet no se pueden graficar varios raster a la vez. Al intentar
  # hacerlo se muestra únicamente el último raster agregado!!
  
  ##############################################################################
  
  ##############################################################################
  ## GRAFICOS CON GGPLOT GGPLOT ####
  
  main_title_split <- unlist(stringr::str_split(main_title, '\n'))
  sub_title <- paste0(paste(tail(main_title_split, -1), collapse = ''))
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  g0 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = pr),
      data = idw.pr.dfr %>%
        dplyr::filter(pr.max.col == 'pr.menor.media'),
      alpha = 1) +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = pr),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.media'),
      alpha = 1) +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = pr),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.mayor.media'),
      alpha = 1) +
    ggplot2::scale_fill_gradientn(
      colours = c(
        RColorBrewer::brewer.pal(4, if (variable == "prcp") 'YlOrBr' else 'GnBu'),
        tail(RColorBrewer::brewer.pal(4, 'Greys'), 4),
        RColorBrewer::brewer.pal(4, if (variable == "prcp") 'BuGn' else 'YlOrBr')),
      breaks = c(100, 200, 300, 400),
      limits = c(100, 400)) + 
    ggplot2::geom_sf(
      data = world,
      fill = "white",
      alpha = 0.05) +
    ggplot2::coord_sf(
      xlim = c(bbox$left-2, bbox$right),
      ylim = c(bbox$bottom, bbox$top+3),
      expand = FALSE) +
    ggplot2::labs(fill = "") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::ggtitle(
      main_title_split[1], subtitle = sub_title) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = gray(0.8), linetype = "dashed", size = 0.5),
      panel.background = ggplot2::element_rect(
        colour = "gray", size = 2, fill = "white"),
      legend.key.height = ggplot2::unit(1, 'cm'),
      legend.position = "none") 
  gg0 <- ggiraph::girafe(
    ggobj = g0 + 
      ggiraph::geom_point_interactive(
        data = idw.pr.dfr %>% 
          dplyr::filter(pr.max.col == 'pr.menor.media'),
        mapping = ggplot2::aes(
          x = lon, y = lat,
          tooltip = pr %% 100),
        alpha = 0.01,
        show.legend = FALSE) + 
      ggiraph::geom_point_interactive(
        data = idw.pr.dfr %>% 
          dplyr::filter(pr.max.col == 'pr.media'),
        mapping = ggplot2::aes(
          x = lon, y = lat,
          tooltip = pr %% 100),
        alpha = 0.01,
        show.legend = FALSE) + 
      ggiraph::geom_point_interactive(
        data = idw.pr.dfr %>% 
          dplyr::filter(pr.max.col == 'pr.mayor.media'),
        mapping = ggplot2::aes(
          x = lon, y = lat,
          tooltip = pr %% 100),
        alpha = 0.01,
        show.legend = FALSE),
    width_svg = 9, 
    height_svg = 10)
  htmlwidgets::saveWidget(
    widget = gg0, 
    file = paste0(fig_file_name, "_ggplot2.html"), 
    selfcontained = TRUE)
  
  g1 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = pr.menor.media * 100),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.menor.media'),
      alpha = 1) +
    ggplot2::scale_fill_gradientn(
      colours = 
        RColorBrewer::brewer.pal(
          4, if (variable == "prcp") 'YlOrBr' else 'GnBu'), 
      limits = c(0, 100),
      breaks = c(33, 40, 50, 60, 70, 80, 90, 100), 
      labels = c("33%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
      na.value = "purple1") + 
    ggplot2::geom_sf(
      data = world,
      fill = "white",
      alpha = 0.05) +
    ggplot2::coord_sf(
      xlim = c(bbox$left-2, bbox$right),
      ylim = c(bbox$bottom, bbox$top+3),
      expand = FALSE) +
    ggplot2::labs(fill = "Below Normal") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::ggtitle(
      main_title_split[1], subtitle = sub_title) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = gray(0.8), linetype = "dashed", size = 0.5),
      panel.background = ggplot2::element_rect(
        colour = "gray", size = 2, fill = "white"),
      legend.key.height = ggplot2::unit(1, 'cm')) 
  gg1 <- ggiraph::girafe(
    ggobj = g1 + 
      ggiraph::geom_point_interactive(
        data = idw.pr.dfr %>% 
          dplyr::filter(pr.max.col == 'pr.menor.media'),
        mapping = ggplot2::aes(
          x = lon, y = lat,
          tooltip = pr.menor.media * 100),
        alpha = 0.01,
        show.legend = FALSE),
    width_svg = 9, 
    height_svg = 10)
  g2 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = pr.media * 100),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.media'),
      alpha = 1) +
    ggplot2::scale_fill_gradientn(
      colours = tail(RColorBrewer::brewer.pal(4, 'Greys'), 4), 
      limits = c(0, 100),
      breaks = c(33, 40, 50, 60, 70, 80, 90, 100), 
      labels = c("33%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
      na.value = "purple1") + 
    ggplot2::geom_sf(
      data = world,
      fill = "white",
      alpha = 0.05) +
    ggplot2::coord_sf(
      xlim = c(bbox$left-2, bbox$right),
      ylim = c(bbox$bottom, bbox$top+3),
      expand = FALSE) +
    ggplot2::labs(fill = "Near Normal") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::ggtitle(
      main_title_split[1], subtitle = sub_title) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = gray(0.8), linetype = "dashed", size = 0.5),
      panel.background = ggplot2::element_rect(
        colour = "gray", size = 2, fill = "white"),
      legend.key.height = ggplot2::unit(1, 'cm')) 
  gg2 <- ggiraph::girafe(
    ggobj = g2 + 
      ggiraph::geom_point_interactive(
        data = idw.pr.dfr %>% 
          dplyr::filter(pr.max.col == 'pr.media'),
        mapping = ggplot2::aes(
          x = lon, y = lat,
          tooltip = pr.media * 100),
        alpha = 0.01,
        show.legend = FALSE),
    width_svg = 9, 
    height_svg = 10)
  g3 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = pr.mayor.media * 100),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.mayor.media'),
      alpha = 1) +
    ggplot2::scale_fill_gradientn(
      colours = 
        RColorBrewer::brewer.pal(
          4, if (variable == "prcp") 'BuGn' else 'YlOrBr'), 
      limits = c(0, 100),
      breaks = c(33, 40, 50, 60, 70, 80, 90, 100), 
      labels = c("33%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
      na.value = "purple1") + 
    ggplot2::geom_sf(
      data = world,
      fill = "white",
      alpha = 0.05) +
    ggplot2::coord_sf(
      xlim = c(bbox$left-2, bbox$right),
      ylim = c(bbox$bottom, bbox$top+3),
      expand = FALSE) +
    ggplot2::labs(fill = "Above Normal") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::ggtitle(
      main_title_split[1], subtitle = sub_title) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = gray(0.8), linetype = "dashed", size = 0.5),
      panel.background = ggplot2::element_rect(
        colour = "gray", size = 2, fill = "white"),
      legend.key.height = ggplot2::unit(1, 'cm')) 
  gg3 <- ggiraph::girafe(
    ggobj = g3 + 
      ggiraph::geom_point_interactive(
        data = idw.pr.dfr %>% 
          dplyr::filter(pr.max.col == 'pr.mayor.media'),
        mapping = ggplot2::aes(
          x = lon, y = lat,
          tooltip = pr.mayor.media * 100),
        alpha = 0.01,
        show.legend = FALSE),
    width_svg = 9, 
    height_svg = 10)
  
  
  #Extract Legend
  g_legend <- function(a.gplot) {
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    if (length(leg) == 0 ) return(NULL)
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  legend_g1 <- g_legend(g1)
  legend_g2 <- g_legend(g2)
  legend_g3 <- g_legend(g3)
  
  
  grDevices::png(filename = paste0(fig_file_name, '.png'), width = 27, 
                 height = 30, units = "cm", res = 600)
  #
  grid::grid.newpage()
  vp <- grid::viewport(width = 0.75, height = 1, x = 0.375, y = .5)
  print(g0, vp = vp)
  #Make the new viewport active and draw
  vp1 <- grid::viewport(width = 0.3, height = 0.3, x = 0.85, y = 0.2)
  grid::upViewport(0)
  grid::pushViewport(vp1)
  grid::grid.draw(legend_g1)
  #Make the new viewport active and draw
  vp2 <- grid::viewport(width = 0.3, height = 0.3, x = 0.845, y = 0.5)
  grid::upViewport(0)
  grid::pushViewport(vp2)
  grid::grid.draw(legend_g2)
  #Make the new viewport active and draw
  vp3 <- grid::viewport(width = 0.3, height = 0.3, x = 0.85, y = 0.8)
  grid::upViewport(0)
  grid::pushViewport(vp3)
  grid::grid.draw(legend_g3)
  #
  grDevices::dev.off()
  
  ##############################################################################
  
}
################################################################################


################################################################################
# Graficar datos (probabilidades separadas - escala continua - op 2)  #####
generar_graficos_prob_sep_continuos_op_2 <- function() {
  
  # Se agrega el PATH al nombre del archivo, pero aún no se define la 
  # extensión, es decir, aún no se define el tipo de archivo.
  fig_file_name <- paste0(config$folders$plots, "continuous_scales/", fig_file_name)
  
  # Definir bounding box de los gráficos (tanto para leaflet como para ggplot2)
  bbox <- list(
    left = config$spatial_domain$wlo, bottom = config$spatial_domain$sla,
    right = config$spatial_domain$elo, top = config$spatial_domain$nla)
  
  ##############################################################################
  ## GRAFICOS CON LEAFLET (https://rstudio.github.io/leaflet/raster.html) ####
  
  idw.pr.raster <-
    raster::rasterFromXYZ(
      xyz = idw.pr.dfr %>%
        dplyr::select(x = lon, y = lat, var = pr)) %>%
    raster::crop(crcsas_sp) %>%
    raster::mask(crcsas_sp)
  raster::crs(idw.pr.raster) <- "EPSG:4326"
  
  idw.pr.raster.obs <-
    raster::rasterFromXYZ(
      xyz = idw.pr.dfr %>%
        dplyr::mutate(var = pr %% 100) %>%
        dplyr::select(x = lon, y = lat, var)) %>%
    raster::crop(crcsas_sp) %>%
    raster::mask(crcsas_sp)
  raster::crs(idw.pr.raster.obs) <- "EPSG:4326"
  
  tag.map.title <- htmltools::tags$style(htmltools::HTML("
      .leaflet-control.map-title {
        text-align: center;
        padding-left: 10px;
        padding-right: 10px;
        font-weight: bold;
        font-size: 14px;
      }
    "))
  title <- htmltools::tags$div(
    tag.map.title,
    htmltools::HTML(stringr::str_replace_all(main_title, '\n', '<br>'))
  )
  library(leaflet)
  library(plainview)
  m <- leaflet::leaflet() %>%
    leaflet::fitBounds(
      lng1 = as.double(bbox$left-2), lng2 = as.double(bbox$right),
      lat1 = as.double(bbox$bottom), lat2 = as.double(bbox$top+3)) %>%
    leaflet::addTiles(
      urlTemplate = paste0("//server.arcgisonline.com/ArcGIS/rest",
                           "/services/World_Street_Map/MapServer",
                           "/tile/{z}/{y}/{x}")) %>%
    leafem::addMouseCoordinates() %>%
    leaflet::addControl(
      html = title,
      position = "topright",
      className="map-title info") %>%
    leaflet::addPolygons(
      data = crcsas_sf,
      stroke = TRUE,
      opacity = 1.0,
      weight = 1,
      fillOpacity = 0.0,
      smoothFactor = 0.5,
      color = "#000000") %>%
    leaflet::addRasterImage(
      x = idw.pr.raster,
      project = TRUE,
      method = "ngb",
      colors = leaflet::colorNumeric(
        palette = c(
          RColorBrewer::brewer.pal(4, if (variable == "prcp") 'YlOrBr' else 'GnBu'),
          tail(RColorBrewer::brewer.pal(4, 'Greys'), 4),
          RColorBrewer::brewer.pal(4, if (variable == "prcp") 'BuGn' else 'YlOrBr')),
        domain = c(100:400),
        na.color = "transparent"),
      opacity = 0.8,
      group = variable_fcst,
      layerId = variable_fcst) %>%
    leafem::addImageQuery(
      x = idw.pr.raster.obs,
      project = TRUE,
      type = "mousemove",
      digits = 1,
      prefix = "",
      group = variable_fcst,
      layerId = variable_fcst,
      position = "topright") %>%
    # leaflet::addCircles(
    #   data = idw.pr.dfr %>%
    #     dplyr::filter(pr.max.col == 'pr.menor.media') %>%
    #     sf::st_as_sf(coords = c('lon', 'lat')) %>%
    #     sf::st_geometry(),
    #   color = if (variable == "prcp") 'red' else 'blue') %>%
    # leaflet::addCircles(
    #   data = idw.pr.dfr %>%
    #     dplyr::filter(pr.max.col == 'pr.media') %>%
    #     sf::st_as_sf(coords = c('lon', 'lat')) %>%
    #     sf::st_geometry(),
  #   color = "grey") %>%
  # leaflet::addCircles(
  #   data = idw.pr.dfr %>%
  #     dplyr::filter(pr.max.col == 'pr.mayor.media') %>%
  #     sf::st_as_sf(coords = c('lon', 'lat')) %>%
  #     sf::st_geometry(),
  #   color = if (variable == "prcp") 'blue' else 'red') %>%
  leaflet::addLegend(
    title = "Above Normal (%)",
    pal = leaflet::colorNumeric(
      palette = 
        RColorBrewer::brewer.pal(4, if (variable == "prcp") 'YlOrBr' else 'GnBu'),
      domain = c(30,40,50,60,70,80,90,100),
      na.color = "transparent"),
    values = c(30,40,50,60,70,80,90,100),
    position = "bottomright") %>%
    leaflet::addLegend(
      title = "Near Normal (%)",
      pal = leaflet::colorNumeric(
        palette = tail(RColorBrewer::brewer.pal(4, 'Greys'), 4),
        domain = c(30,40,50,60,70,80,90,100),
        na.color = "transparent"),
      values = c(30,40,50,60,70,80,90,100),
      position = "bottomright") %>%
    leaflet::addLegend(
      title = "Below Normal (%)",
      pal = leaflet::colorNumeric(
        palette = 
          RColorBrewer::brewer.pal(4, if (variable == "prcp") 'BuGn' else 'YlOrBr'),
        domain = c(30,40,50,60,70,80,90,100),
        na.color = "transparent"),
      values = c(30,40,50,60,70,80,90,100),
      position = "bottomright") %>%
    leaflet::addControl(
      html = GenerarHTMLLogo(
        paste0(config$folders$images, "logo-crcsas.png")),
      position = "bottomleft") %>%
    leaflet::addSimpleGraticule() %>%
    leaflet.extras2::addEasyprint(
      options = leaflet.extras2::easyprintOptions(
        title = "Descargar mapa a PNG",
        sizeModes = list("A4Portrait", "A4Landscape"),
        exportOnly = TRUE,
        hideControlContainer = FALSE,
        filename = fig_file_name))
  htmlwidgets::saveWidget(
    widget = m,
    file = paste0(fig_file_name, "_leaflet.html"),
    selfcontained = TRUE)
  # mapview::mapshot(m, file = paste0(fig_file_name, ".png"))
  
  ##############################################################################
  
  ##############################################################################
  ## GRAFICOS CON GGPLOT GGPLOT ####
  
  # LA ÚNICA DIFERENCIA CON LA OPCIÓN 1 ES QUE ESTA OPCIÓN 
  # USA UN SOLO ggplot2::geom_raster
  
  # main_title_split <- unlist(stringr::str_split(main_title, '\n'))
  # sub_title <- paste0(paste(tail(main_title_split, -1), collapse = ''))
  # world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  # 
  # g0 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
  #   ggplot2::geom_raster(
  #     mapping = ggplot2::aes(
  #       x = lon, y = lat,
  #       fill = pr),
  #     data = idw.pr.dfr,
  #     alpha = 1) +
  #   ggplot2::scale_fill_gradientn(
  #     colours = c(
  #       RColorBrewer::brewer.pal(4, if (variable == "prcp") 'YlOrBr' else 'GnBu'),
  #       tail(RColorBrewer::brewer.pal(4, 'Greys'), 4),
  #       RColorBrewer::brewer.pal(4, if (variable == "prcp") 'BuGn' else 'YlOrBr')),
  #     breaks = c(100, 200, 300, 400),
  #     limits = c(100, 400)) + 
  #   ggplot2::geom_sf(
  #     data = world,
  #     fill = "white",
  #     alpha = 0.05) +
  #   ggplot2::coord_sf(
  #     xlim = c(bbox$left-2, bbox$right),
  #     ylim = c(bbox$bottom, bbox$top+3),
  #     expand = FALSE) +
  #   ggplot2::labs(fill = "") +
  #   ggplot2::xlab("") +
  #   ggplot2::ylab("") +
  #   ggplot2::ggtitle(
  #     main_title_split[1], subtitle = sub_title) +
  #   ggplot2::theme(
  #     panel.grid.major = ggplot2::element_line(
  #       color = gray(0.8), linetype = "dashed", size = 0.5),
  #     panel.background = ggplot2::element_rect(
  #       colour = "gray", size = 2, fill = "white"),
  #     legend.key.height = ggplot2::unit(1, 'cm'),
  #     legend.position = "none") 
  # gg0 <- ggiraph::girafe(
  #   ggobj = g0 + 
  #     ggiraph::geom_point_interactive(
  #       data = idw.pr.dfr %>% 
  #         dplyr::filter(pr.max.col == 'pr.menor.media'),
  #       mapping = ggplot2::aes(
  #         x = lon, y = lat,
  #         tooltip = pr %% 100),
  #       alpha = 0.01,
  #       show.legend = FALSE) + 
  #     ggiraph::geom_point_interactive(
  #       data = idw.pr.dfr %>% 
  #         dplyr::filter(pr.max.col == 'pr.media'),
  #       mapping = ggplot2::aes(
  #         x = lon, y = lat,
  #         tooltip = pr %% 100),
  #       alpha = 0.01,
  #       show.legend = FALSE) + 
  #     ggiraph::geom_point_interactive(
  #       data = idw.pr.dfr %>% 
  #         dplyr::filter(pr.max.col == 'pr.mayor.media'),
  #       mapping = ggplot2::aes(
  #         x = lon, y = lat,
  #         tooltip = pr %% 100),
  #       alpha = 0.01,
  #       show.legend = FALSE),
  #   width_svg = 9, 
  #   height_svg = 10)
  # htmlwidgets::saveWidget(
  #   widget = gg0, 
  #   file = paste0(fig_file_name, "_ggplot2.html"), 
  #   selfcontained = TRUE)
  # 
  # g1 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
  #   ggplot2::geom_raster(
  #     mapping = ggplot2::aes(
  #       x = lon, y = lat,
  #       fill = pr.menor.media * 100),
  #     data = idw.pr.dfr %>% 
  #       dplyr::filter(pr.max.col == 'pr.menor.media'),
  #     alpha = 1) +
  #   ggplot2::scale_fill_gradientn(
  #     colours = 
  #       RColorBrewer::brewer.pal(
  #         4, if (variable == "prcp") 'YlOrBr' else 'GnBu'), 
  #     limits = c(0, 100),
  #     breaks = c(33, 40, 50, 60, 70, 80, 90, 100), 
  #     labels = c("33%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
  #     na.value = "purple1") + 
  #   ggplot2::geom_sf(
  #     data = world,
  #     fill = "white",
  #     alpha = 0.05) +
  #   ggplot2::coord_sf(
  #     xlim = c(bbox$left-2, bbox$right),
  #     ylim = c(bbox$bottom, bbox$top+3),
  #     expand = FALSE) +
  #   ggplot2::labs(fill = "Below Normal") +
  #   ggplot2::xlab("") +
  #   ggplot2::ylab("") +
  #   ggplot2::ggtitle(
  #     main_title_split[1], subtitle = sub_title) +
  #   ggplot2::theme(
  #     panel.grid.major = ggplot2::element_line(
  #       color = gray(0.8), linetype = "dashed", size = 0.5),
  #     panel.background = ggplot2::element_rect(
  #       colour = "gray", size = 2, fill = "white"),
  #     legend.key.height = ggplot2::unit(1, 'cm')) 
  # gg1 <- ggiraph::girafe(
  #   ggobj = g1 + 
  #     ggiraph::geom_point_interactive(
  #       data = idw.pr.dfr %>% 
  #         dplyr::filter(pr.max.col == 'pr.menor.media'),
  #       mapping = ggplot2::aes(
  #         x = lon, y = lat,
  #         tooltip = pr.menor.media * 100),
  #       alpha = 0.01,
  #       show.legend = FALSE),
  #   width_svg = 9, 
  #   height_svg = 10)
  # g2 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
  #   ggplot2::geom_raster(
  #     mapping = ggplot2::aes(
  #       x = lon, y = lat,
  #       fill = pr.media * 100),
  #     data = idw.pr.dfr %>% 
  #       dplyr::filter(pr.max.col == 'pr.media'),
  #     alpha = 1) +
  #   ggplot2::scale_fill_gradientn(
  #     colours = tail(RColorBrewer::brewer.pal(4, 'Greys'), 4), 
  #     limits = c(0, 100),
  #     breaks = c(33, 40, 50, 60, 70, 80, 90, 100), 
  #     labels = c("33%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
  #     na.value = "purple1") + 
  #   ggplot2::geom_sf(
  #     data = world,
  #     fill = "white",
  #     alpha = 0.05) +
  #   ggplot2::coord_sf(
  #     xlim = c(bbox$left-2, bbox$right),
  #     ylim = c(bbox$bottom, bbox$top+3),
  #     expand = FALSE) +
  #   ggplot2::labs(fill = "Near Normal") +
  #   ggplot2::xlab("") +
  #   ggplot2::ylab("") +
  #   ggplot2::ggtitle(
  #     main_title_split[1], subtitle = sub_title) +
  #   ggplot2::theme(
  #     panel.grid.major = ggplot2::element_line(
  #       color = gray(0.8), linetype = "dashed", size = 0.5),
  #     panel.background = ggplot2::element_rect(
  #       colour = "gray", size = 2, fill = "white"),
  #     legend.key.height = ggplot2::unit(1, 'cm')) 
  # gg2 <- ggiraph::girafe(
  #   ggobj = g2 + 
  #     ggiraph::geom_point_interactive(
  #       data = idw.pr.dfr %>% 
  #         dplyr::filter(pr.max.col == 'pr.media'),
  #       mapping = ggplot2::aes(
  #         x = lon, y = lat,
  #         tooltip = pr.media * 100),
  #       alpha = 0.01,
  #       show.legend = FALSE),
  #   width_svg = 9, 
  #   height_svg = 10)
  # g3 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
  #   ggplot2::geom_raster(
  #     mapping = ggplot2::aes(
  #       x = lon, y = lat,
  #       fill = pr.mayor.media * 100),
  #     data = idw.pr.dfr %>% 
  #       dplyr::filter(pr.max.col == 'pr.mayor.media'),
  #     alpha = 1) +
  #   ggplot2::scale_fill_gradientn(
  #     colours = 
  #       RColorBrewer::brewer.pal(
  #         4, if (variable == "prcp") 'BuGn' else 'YlOrBr'), 
  #     limits = c(0, 100),
  #     breaks = c(33, 40, 50, 60, 70, 80, 90, 100), 
  #     labels = c("33%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
  #     na.value = "purple1") + 
  #   ggplot2::geom_sf(
  #     data = world,
  #     fill = "white",
  #     alpha = 0.05) +
  #   ggplot2::coord_sf(
  #     xlim = c(bbox$left-2, bbox$right),
  #     ylim = c(bbox$bottom, bbox$top+3),
  #     expand = FALSE) +
  #   ggplot2::labs(fill = "Above Normal") +
  #   ggplot2::xlab("") +
  #   ggplot2::ylab("") +
  #   ggplot2::ggtitle(
  #     main_title_split[1], subtitle = sub_title) +
  #   ggplot2::theme(
  #     panel.grid.major = ggplot2::element_line(
  #       color = gray(0.8), linetype = "dashed", size = 0.5),
  #     panel.background = ggplot2::element_rect(
  #       colour = "gray", size = 2, fill = "white"),
  #     legend.key.height = ggplot2::unit(1, 'cm')) 
  # gg3 <- ggiraph::girafe(
  #   ggobj = g3 + 
  #     ggiraph::geom_point_interactive(
  #       data = idw.pr.dfr %>% 
  #         dplyr::filter(pr.max.col == 'pr.mayor.media'),
  #       mapping = ggplot2::aes(
  #         x = lon, y = lat,
  #         tooltip = pr.mayor.media * 100),
  #       alpha = 0.01,
  #       show.legend = FALSE),
  #   width_svg = 9, 
  #   height_svg = 10)
  # 
  # 
  # #Extract Legend
  # g_legend <- function(a.gplot) {
  #   tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
  #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  #   if (length(leg) == 0 ) return(NULL)
  #   legend <- tmp$grobs[[leg]]
  #   return(legend)
  # }
  # legend_g1 <- g_legend(g1)
  # legend_g2 <- g_legend(g2)
  # legend_g3 <- g_legend(g3)
  # 
  # 
  # grDevices::png(filename = paste0(fig_file_name, '.png'), width = 27, 
  #                height = 30, units = "cm", res = 600)
  # #
  # grid::grid.newpage()
  # vp <- grid::viewport(width = 0.75, height = 1, x = 0.375, y = .5)
  # print(g0, vp = vp)
  # #Make the new viewport active and draw
  # vp1 <- grid::viewport(width = 0.3, height = 0.3, x = 0.85, y = 0.2)
  # grid::upViewport(0)
  # grid::pushViewport(vp1)
  # grid::grid.draw(legend_g1)
  # #Make the new viewport active and draw
  # vp2 <- grid::viewport(width = 0.3, height = 0.3, x = 0.845, y = 0.5)
  # grid::upViewport(0)
  # grid::pushViewport(vp2)
  # grid::grid.draw(legend_g2)
  # #Make the new viewport active and draw
  # vp3 <- grid::viewport(width = 0.3, height = 0.3, x = 0.85, y = 0.8)
  # grid::upViewport(0)
  # grid::pushViewport(vp3)
  # grid::grid.draw(legend_g3)
  # #
  # grDevices::dev.off()
  
  ##############################################################################
  
}
################################################################################