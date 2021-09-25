

################################################################################
## Generar etiquetas en base a grupos ##################
generar_etiquetas_grupos <- function(grupos, formato.numero = "%.2f") {
  
  etiquetas <- purrr::map(
    .x = grupos,
    .f = function(grupo) {
      g_i <- stringr::str_sub(grupo, +1, +1)
      g_f <- stringr::str_sub(grupo, -1, -1)
      g_n <- stringr::str_replace_all(grupo, "[\\[\\]\\(\\)]", "")
      desde <- as.numeric(stringr::str_split_fixed(g_n, ",", 2)[1])
      hasta <- as.numeric(stringr::str_split_fixed(g_n, ",", 2)[2])
      
      if (is.infinite(desde) && is.finite(hasta)) {
        if (desde < hasta) {
          if (g_f == ")")
            return (sprintf(paste0("< ", formato.numero), hasta))
          if (g_f == "]")
            return (sprintf(paste0("<= ", formato.numero), hasta))
        }
        if (desde > hasta) {
          if (g_f == ")")
            return (sprintf(paste0("> ", formato.numero), hasta))
          if (g_f == "]")
            return (sprintf(paste0(">= ", formato.numero), hasta))
        }
      }
      
      if (is.finite(desde) && is.infinite(hasta)) {
        if (desde < hasta) {
          if (g_i == "(")
            return (sprintf(paste0("> ", formato.numero), desde))
          if (g_i == "[")
            return (sprintf(paste0(">= ", formato.numero), desde))
        }
        if (desde > hasta) {
          if (g_i == "(")
            return (sprintf(paste0("< ", formato.numero), desde))
          if (g_i == "[")
            return (sprintf(paste0("<= ", formato.numero), desde))
        }
      }
      
      if (is.finite(desde) && is.finite(hasta)) {
        if (desde < hasta) {
          if (g_i == "(" && g_f == ")")
            return (sprintf(paste0(formato.numero, "+ .. ", formato.numero, "(-)"), desde, hasta))
          if (g_i == "[" && g_f == ")")
            return (sprintf(paste0(formato.numero, " .. ", formato.numero, "(-)"), desde, hasta))
          if (g_i == "(" && g_f == "]")
            return (sprintf(paste0(formato.numero, "+ .. ", formato.numero), desde, hasta))
          if (g_i == "[" && g_f == "]")
            return (sprintf(paste0(formato.numero, " .. ", formato.numero), desde, hasta))
        }
        if (desde > hasta) {
          if (g_i == "(" && g_f == ")")
            return (sprintf(paste0(formato.numero, "- .. ", formato.numero, "(+)"), desde, hasta))
          if (g_i == "[" && g_f == ")")
            return (sprintf(paste0(formato.numero, " .. ", formato.numero, "(+)"), desde, hasta))
          if (g_i == "(" && g_f == "]")
            return (sprintf(paste0(formato.numero, "- .. ", formato.numero), desde, hasta))
          if (g_i == "[" && g_f == "]")
            return (sprintf(paste0(formato.numero, " .. ", formato.numero), desde, hasta))
        }
      }
      
    }) %>% unlist()
  
  return (etiquetas)
}
################################################################################

################################################################################
## Definir grupos ##################
definir_grupos <- function(breaks, include.lowest = FALSE, right = TRUE) {
  
  groups <-
    levels(cut(breaks, breaks, include.lowest = include.lowest, right = right))
  
  return (groups)
}
################################################################################

################################################################################
## Asignar grupos ##################
asignar_grupos <- function(dfr, breaks, groups, include.lowest = FALSE, right = TRUE) {
  
  grouped.dfr <- dfr %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      grupo = as.character(cut(var, breaks, 
                               include.lowest = include.lowest, 
                               right = right))) %>%
    dplyr::mutate(
      indice_grupo = which(groups == grupo)
    ) %>%
    dplyr::ungroup()
  
  return (grouped.dfr)
}
################################################################################

################################################################################
## Definir grupos simetricos ##################
definir_grupos_simetricos <- function(breaks, n_head, n_tail, include.lowest = F) {
  
  groups <- c(
    levels(cut(head(breaks, n_head), head(breaks, n_head), 
               include.lowest = include.lowest, right = T)),
    paste0("(", 
           tail(head(breaks, n_head), 1), ",", head(tail(breaks, n_tail), 1), 
           ")"),
    levels(cut(tail(breaks, n_tail), tail(breaks, n_tail), 
               include.lowest = include.lowest, right = F)))
  
  return (groups)
}
################################################################################

################################################################################
## Asignar grupos simetricos ##################
asignar_grupos_simetricos <- function(dfr, breaks, n_head, n_tail, groups,
                                      include.lowest = F) {
  
  grouped.dfr <- dfr %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      grupo = dplyr::if_else(
        var <= tail(head(breaks, n_head), 1), 
        as.character(cut(var, head(breaks, n_head), 
                         include.lowest = include.lowest, right = T)), 
        NA_character_)) %>%
    dplyr::mutate(
      grupo = dplyr::if_else(
        var >= head(tail(breaks, n_tail), 1), 
        as.character(cut(var, tail(breaks, n_tail),
                         include.lowest = include.lowest, right = F)), 
        grupo)) %>%
    dplyr::mutate(
      grupo = dplyr::if_else(
        is.na(grupo),
        paste0("(", 
               tail(head(breaks, n_head), 1), ",", head(tail(breaks, n_tail), 1), 
               ")"),
        grupo)) %>%
    dplyr::mutate(
      indice_grupo = which(groups == grupo)
    ) %>%
    dplyr::ungroup()
  
  return (grouped.dfr)
}
################################################################################

################################################################################
## AGeneracion de HTML con logo ##################
GenerarHTMLLogo <- function(logo.file) {
  logo.ascii <- base::readBin(con = logo.file,
                              what = "raw",
                              n = base::file.info(logo.file)[1, "size"])
  logo.b64   <- RCurl::base64Encode(txt = logo.ascii,
                                    mode = "character")
  html       <- paste0("<img src='data:image/png;base64,", logo.b64,
                       "' border=\"0\" alt=\"CRC-SAS\"/>")
  return (html)
}
################################################################################


################################################################################
# Graficar datos (graficos de Fabricio) ####
generar_graficos_fabricio <- function() {
  
  # Se agrega el PATH al nombre del archivo, pero aún no se define la 
  # extensión, es decir, aún no se define el tipo de archivo.
  fig_file_name <- paste0(config$folders$plots, "fabricio/", fig_file_name)
  
  ##############################################################################
  ## GRAFICOS DE FABRICIO ####
  library(lattice, quietly = TRUE)
  
  if (data_type == "anom") {
    if (variable == 'prcp') {
      seqq <- c(-200,-100,-50,-20,-10,-5,5,10,20,50,100,200)
      seq.l <- c(-200,-100,-50,-20,-10,-5,5,10,20,50,100,200)
    } else if (variable == 't2m') {
      seqq <- c(-4,-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2,4)
      seq.l <- c(-4,-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2,4)
    }
    paleta <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7',
                '#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac',
                '#053061','#1d0036')
    paleta <- if (variable == 't2m') rev(paleta) else paleta
    teste <- grDevices::colorRampPalette(paleta)
    par_settings <- rasterVis::rasterTheme(region=teste(14))
    col_regions  <- NULL
  } else if (data_type == "corr") {
    seqq <- c(-1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
    seq.l <- c(-1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
    paleta <- c("#ff6766","#ff9899","#fecccb","#99ffff","#00ffff",
                "#00ccff","#99cccd","#6599ff","#6665fe","#0000fe",
                "#010189")
    teste <- grDevices::colorRampPalette(paleta)
    par_settings <- rasterVis::rasterTheme(region=teste(11))
    col_regions  <- NULL
  } else if (data_type == "value.gen") {  # PREV_PREC
    if (variable == 'prcp') {
      seqq <- c(0,1,25,50,100,150,200,250,300,400,500)
      seq.l <- c(0,1,25,50,100,150,200,250,300,400,500)
      paleta <- c("#ff6634","#ff9934","#ffcc00","#ffffcd","#cdffcc",
                  "#9acc99","#34cc67","#33cc33","#019934","#006634",
                  "#00381d")
    } else if (variable == 't2m') {
      seqq <- c(-10,8,10,12,14,16,18,20,22,24,26,28,30,32)
      seq.l <- c(-10,8,10,12,14,16,18,20,22,24,26,28,30,32)
      paleta <- c("#9a99ff","#ccccfe","#99ffff","#cdffcc","#ffffcb",
                  "#ffcb99","#ffcc00","#ff9a66","#ff9934","#cd9933",
                  "#cc6733","#ff6634","#fe0000","#c10202")
    }
    teste <- grDevices::colorRampPalette(paleta)
    par_settings <- rasterVis::rasterTheme(region=teste(10))
    col_regions  <- NULL
  } else if (data_type == "codigo_de_color") {  # PROB_PREC
    seqq <- c(-6,-5,-4,-3,-2,-1,1,2,3,4,5,6)
    seq.l <- c("70","60","50","45","40","35","35","40","45","50","60","70")
    if (variable == 'prcp') {
      paleta <- c("#783301","#a9461d","#cf8033","#e8b832","#fafb01",
                  "#c8c8c8","#c5ffe7","#96feaf","#67ff78","#35e43f",
                  "#06c408","#018d03")
    } else if (variable == 't2m') {
      paleta <- c("#6665fe","#6599ff","#9a99ff","#00ccff","#99ffff",
                  "#eaf6f6","#fafaf3","#feff99","#ffff66","#ffcc00",
                  "#ff9a66","#fb6c22")
    }
    prob.b <- colorRampPalette(paleta)
    par_settings <- list(prob.b, layout.heights = list(xlab.key.padding=2.5))
    col_regions <- prob.b(11)
  } else if (data_type == "value.fcst") {  
    if (variable == 'prcp') {
      seqq <- c(0,1,25,50,100,150,200,250,300,400,500)
      seq.l <- c(0,1,25,50,100,150,200,250,300,400,500)
      paleta <- c("#ff6634","#ff9934","#ffcc00","#ffffcd","#cdffcc",
                  "#9acc99","#34cc67","#33cc33","#019934","#006634",
                  "#00381d")
    } else if (variable == 't2m') {
      seqq <- c(-10,8,10,12,14,16,18,20,22,24,26,28,30,32)
      seq.l <- c(-10,8,10,12,14,16,18,20,22,24,26,28,30,32)
      paleta <- c("#9a99ff","#ccccfe","#99ffff","#cdffcc","#ffffcb",
                  "#ffcb99","#ffcc00","#ff9a66","#ff9934","#cd9933",
                  "#cc6733","#ff6634","#fe0000","#c10202")
    }
    teste <- grDevices::colorRampPalette(paleta)
    par_settings <- rasterVis::rasterTheme(region=teste(10))
    col_regions  <- NULL
  }
  
  seq.interval <- seq(1, length(seqq), by=1)
  myColorkey <- list(
    at = seq.interval, 
    labels = list(
      at = seq.interval, 
      labels = as.character(seq.l)), 
    space = "bottom",
    width = 1)
  
  BELOW <- lattice::levelplot(x = var ~ x * y, 
                              data = idw.msk.dfr,
                              contour = F,
                              at = seqq,
                              colorkey = myColorkey,
                              par.settings = par_settings,
                              col.regions = col_regions,
                              main = main_title,
                              xlab = NULL, ylab = NULL) +
    latticeExtra::layer(sp::sp.lines(paises_sp, alpha=1))
  grDevices::jpeg(filename = paste0(fig_file_name, '.jpg'), width = 16, 
                  height = 21, quality = 75, units = "cm", res = 300)
  print(BELOW)
  if (data_type == "codigo_de_color") {
    grid::grid.text('Near', rot=0, y=grid::unit(0.09, "npc"), 
                    x=grid::unit(0.512, "npc"))
    grid::grid.text('Below', rot=0, y=grid::unit(0.09, "npc"), 
                    x=grid::unit(0.28, "npc"))
    grid::grid.text('Above', rot=0, y=grid::unit(0.09, "npc"), 
                    x=grid::unit(0.75, "npc"))
  }
  grDevices::dev.off()
  
  ##############################################################################
  
}
################################################################################

################################################################################
# Graficar datos (escalas discretas)  #####
generar_graficos_discretos <- function() {
  
  # Se agrega el PATH al nombre del archivo, pero aún no se define la 
  # extensión, es decir, aún no se define el tipo de archivo.
  fig_file_name <- paste0(config$folders$plots, "discrete_scales/", fig_file_name)
  
  # Definir bounding box de los gráficos (tanto para leaflet como para ggplot2)
  bbox <- list(
    left = config$spatial_domain$wlo, bottom = config$spatial_domain$sla,
    right = config$spatial_domain$elo, top = config$spatial_domain$nla)
    
  ##############################################################################
  ## GRAFICOS CON LEAFLET (https://rstudio.github.io/leaflet/raster.html) ####
  
  if (data_type == "anom") {
    if (variable == 'prcp') {
      breaks <- c(-Inf,-200,-100,-50,-20,-10,-5,5,10,20,50,100,200,Inf)
      groups <- definir_grupos_simetricos(breaks, 7, 7)
      domain <- c(1:length(groups))
      labels <- generar_etiquetas_grupos(groups, "%d")
      legend_labels <- labels
      grouped.idw.msk.dfr <- 
        asignar_grupos_simetricos(idw.msk.dfr, breaks, 7, 7, groups) 
    } else if (variable == 't2m') {
      breaks <- c(-Inf,-4,-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2,4,Inf)
      groups <- definir_grupos_simetricos(breaks, 7, 7)
      domain <- c(1:length(groups))
      labels <- generar_etiquetas_grupos(groups, "%.1f")
      legend_labels <- labels
      grouped.idw.msk.dfr <- 
        asignar_grupos_simetricos(idw.msk.dfr, breaks, 7, 7, groups) 
    }
    paleta <- grDevices::colorRampPalette(
      colors = RColorBrewer::brewer.pal(11, 'RdBu'))( 13 )
    paleta <- if (variable == 't2m') rev(paleta) else paleta
    legend_paleta <- paleta
    grouped.idw.msk <- raster::rasterFromXYZ(
      xyz = grouped.idw.msk.dfr %>% dplyr::select(x, y, indice_grupo)) %>%
      raster::crop(crcsas_sp) %>% raster::mask(crcsas_sp) 
    raster::crs(grouped.idw.msk) <- "EPSG:4326"
  } else if (data_type == "corr") {
    breaks <- c(-1,-0.5,-0.1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
    groups <- definir_grupos_simetricos(breaks, 3, 10, include.lowest = T)
    domain <- c(1:length(groups))
    labels <- generar_etiquetas_grupos(groups, "%.1f")
    legend_labels <- labels
    grouped.idw.msk.dfr <- 
      asignar_grupos_simetricos(idw.msk.dfr, breaks, 3, 10, groups, include.lowest = T)
    red_plt  <- head(rev(RColorBrewer::brewer.pal(3, 'Reds')), 2)
    blue_plt  <- tail(grDevices::colorRampPalette(
      colors = RColorBrewer::brewer.pal(9, 'Blues'))( 11 ), 9)
    paleta <- c(red_plt, "#ffffff", blue_plt) 
    legend_paleta <- paleta
    grouped.idw.msk <- raster::rasterFromXYZ(
      xyz = grouped.idw.msk.dfr %>% dplyr::select(x, y, indice_grupo)) %>%
      raster::crop(crcsas_sp) %>% raster::mask(crcsas_sp) 
    raster::crs(grouped.idw.msk) <- "EPSG:4326"
  } else if (data_type == "value.gen") {  # PREV_PREC
    if (variable == 'prcp') {
      breaks <- c(0,1,25,50,100,150,200,250,300,400,500,600,Inf)
      groups <- definir_grupos(breaks, include.lowest = TRUE, right = FALSE)
      domain <- c(1:length(groups))
      grouped.idw.msk.dfr <- 
        asignar_grupos(idw.msk.dfr, breaks, groups, include.lowest = TRUE, right = FALSE) 
      paleta <- grDevices::colorRampPalette(
        colors = RColorBrewer::brewer.pal(9, 'Blues'))( 12 )
      legend_paleta <- paleta
    } else if (variable == 't2m') {
      breaks <- c(-Inf,-9,-6,-3,3,6,9,12,15,18,21,24,27,30,33,Inf)
      groups <- definir_grupos_simetricos(breaks, 4, 12)
      domain <- c(1:length(groups))
      grouped.idw.msk.dfr <- 
        asignar_grupos_simetricos(idw.msk.dfr, breaks, 4, 12, groups) 
      # paleta <- grDevices::colorRampPalette(
      #   colors = RColorBrewer::brewer.pal(9, 'Oranges'))( 16 )
      # paleta <- tail(viridis::turbo(22), 16)
      paleta <- viridis::plasma(15)
      legend_paleta <- paleta
    }
    labels <- generar_etiquetas_grupos(groups, "%d")
    legend_labels <- labels
    grouped.idw.msk <- raster::rasterFromXYZ(
      xyz = grouped.idw.msk.dfr %>% dplyr::select(x, y, indice_grupo)) %>%
      raster::crop(crcsas_sp) %>% raster::mask(crcsas_sp) 
    raster::crs(grouped.idw.msk) <- "EPSG:4326"
  } else if (data_type == "value.fcst") {  
    if (variable == 'prcp') {
      breaks <- c(0,1,25,50,100,150,200,250,300,400,500,600,Inf)
      groups <- definir_grupos(breaks, include.lowest = TRUE, right = FALSE)
      domain <- c(1:length(groups))
      grouped.idw.msk.dfr <- 
        asignar_grupos(idw.msk.dfr, breaks, groups, include.lowest = TRUE, right = FALSE) 
      paleta <- grDevices::colorRampPalette(
        colors = RColorBrewer::brewer.pal(9, 'Blues'))( 12 )
      legend_paleta <- paleta
    } else if (variable == 't2m') {
      breaks <- c(-Inf,-9,-6,-3,3,6,9,12,15,18,21,24,27,30,33,Inf)
      groups <- definir_grupos_simetricos(breaks, 4, 12)
      domain <- c(1:length(groups))
      grouped.idw.msk.dfr <- 
        asignar_grupos_simetricos(idw.msk.dfr, breaks, 4, 12, groups) 
      # paleta <- grDevices::colorRampPalette(
      #   colors = RColorBrewer::brewer.pal(9, 'Oranges'))( 16 )
      # paleta <- tail(viridis::turbo(22), 16)
      paleta <- viridis::plasma(15)
      legend_paleta <- paleta
    }
    labels <- generar_etiquetas_grupos(groups, "%d")
    legend_labels <- labels
    grouped.idw.msk <- raster::rasterFromXYZ(
      xyz = grouped.idw.msk.dfr %>% dplyr::select(x, y, indice_grupo)) %>%
      raster::crop(crcsas_sp) %>% raster::mask(crcsas_sp) 
    raster::crs(grouped.idw.msk) <- "EPSG:4326"
  } else {
    return (NULL)
  }
  
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
    # leafem::addMouseCoordinates() %>%
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
      x = grouped.idw.msk, 
      project = TRUE,
      colors = leaflet::colorNumeric(
        palette = paleta, 
        domain = domain,
        na.color = "transparent"),
      opacity = 0.8, 
      group = variable_fcst,
      layerId = variable_fcst)  %>%
    leafem::addImageQuery(
      x = idw.msk, 
      project = TRUE,
      type = "mousemove", 
      digits = 1, 
      prefix = "",
      group = variable_fcst,
      layerId = variable_fcst,
      position = "topright") %>%
    # leaflet::addCircles(
    #   data = idw.msk.sf) %>%
    leaflet::addLegend(
      title = variable_str,
      colors = legend_paleta,
      labels = legend_labels,
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
  if (data_type == "anom") {
    if (variable == 'prcp') {
      breaks <- c(-Inf,-200,-100,-50,-20,-10,-5,5,10,20,50,100,200,Inf)
      groups <- definir_grupos_simetricos(breaks, 7, 7)
      labels <- generar_etiquetas_grupos(groups, "%d")
      grouped.idw.msk.dfr <- asignar_grupos_simetricos(idw.msk.dfr, breaks, 7, 7, groups)
    } else if (variable == 't2m') {
      breaks <- c(-Inf,-4,-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2,4,Inf)
      groups <- definir_grupos_simetricos(breaks, 7, 7)
      labels <- generar_etiquetas_grupos(groups, "%.1f")
      grouped.idw.msk.dfr <- asignar_grupos_simetricos(idw.msk.dfr, breaks, 7, 7, groups)
    }
    paleta <- grDevices::colorRampPalette(
      colors = RColorBrewer::brewer.pal(11, 'RdBu'))( 13 )
    paleta <- if (variable == 't2m') rev(paleta) else paleta
  } else if (data_type == "corr") {
    breaks <- c(-1,-0.5,-0.1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
    groups <- definir_grupos_simetricos(breaks, 3, 10, include.lowest = T)
    labels <- generar_etiquetas_grupos(groups, "%.1f")
    grouped.idw.msk.dfr <- 
      asignar_grupos_simetricos(idw.msk.dfr, breaks, 3, 10, groups, include.lowest = T)
    red_plt  <- head(rev(RColorBrewer::brewer.pal(3, 'Reds')), 2)
    blue_plt  <- tail(grDevices::colorRampPalette(
      colors = RColorBrewer::brewer.pal(9, 'Blues'))( 11 ), 9)
    paleta <- c(red_plt, "#ffffff", blue_plt) 
  } else if (data_type == "value.gen") {  # PREV_PREC
    if (variable == 'prcp') {
      breaks <- c(0,1,25,50,100,150,200,250,300,400,500,600,Inf)
      groups <- definir_grupos(breaks, include.lowest = TRUE, right = FALSE)
      grouped.idw.msk.dfr <- 
        asignar_grupos(idw.msk.dfr, breaks, groups, include.lowest = TRUE, right = FALSE) 
      paleta <- grDevices::colorRampPalette(
        colors = RColorBrewer::brewer.pal(9, 'Blues'))( 12 )
    } else if (variable == 't2m') {
      breaks <- c(-Inf,-9,-6,-3,0,3,6,9,12,15,18,21,24,27,30,33,Inf)
      groups <- definir_grupos_simetricos(breaks, 4, 12)
      grouped.idw.msk.dfr <- 
        asignar_grupos_simetricos(idw.msk.dfr, breaks, 4, 12, groups) 
      # paleta <- grDevices::colorRampPalette(
      #   colors = RColorBrewer::brewer.pal(9, 'Oranges'))( 16 )
      # paleta <- tail(viridis::turbo(22), 16)
      paleta <- viridis::plasma(15)
    }
    labels <- generar_etiquetas_grupos(groups, "%d")
  } else if (data_type == "value.fcst") {  
    if (variable == 'prcp') {
      breaks <- c(0,1,25,50,100,150,200,250,300,400,500,600,Inf)
      groups <- definir_grupos(breaks, include.lowest = TRUE, right = FALSE)
      grouped.idw.msk.dfr <- 
        asignar_grupos(idw.msk.dfr, breaks, groups, include.lowest = TRUE, right = FALSE) 
      paleta <- grDevices::colorRampPalette(
        colors = RColorBrewer::brewer.pal(9, 'Blues'))( 12 )
    } else if (variable == 't2m') {
      breaks <- c(-Inf,-9,-6,-3,0,3,6,9,12,15,18,21,24,27,30,33,Inf)
      groups <- definir_grupos_simetricos(breaks, 4, 12)
      grouped.idw.msk.dfr <- 
        asignar_grupos_simetricos(idw.msk.dfr, breaks, 4, 12, groups) 
      # paleta <- grDevices::colorRampPalette(
      #   colors = RColorBrewer::brewer.pal(9, 'Oranges'))( 16 )
      # paleta <- tail(viridis::turbo(22), 16)
      paleta <- viridis::plasma(15)
    }
    labels <- generar_etiquetas_grupos(groups, "%d")
  } else {
    return (NULL)
  }
  
  main_title_split <- unlist(stringr::str_split(main_title, '\n'))
  sub_title <- paste0(paste(tail(main_title_split, -1), collapse = ''))
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  g <- ggplot2::ggplot(data = grouped.idw.msk.dfr, 
                       width = 27, 
                       height = 30, 
                       units = "cm") +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = x, y = y,
        fill = factor(grupo, levels = groups)),
      alpha = 1) +
    ggplot2::scale_discrete_manual(
      aesthetics = "fill",
      breaks = groups,
      values = paleta,
      labels = labels,
      drop = FALSE) +
    # ggplot2::scale_fill_continuous(
    #   limits=c(min(seqq), max(seqq)), breaks=seqq) +
    # ggplot2::scale_fill_gradientn(
    #   colours = paleta, limits=c(min(seqq), max(seqq)),
    #   breaks = seqq, labels = format(seq.l)) +
    # ggplot2::scale_fill_gradient2(
    #   low = "blue", mid = "white", high = "red",
    #   midpoint = mean(idw.msk.dfr$var)) +
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
    # ggspatial::annotation_scale(
    #   location = "bl", width_hint = 0.4) +
    # ggspatial::annotation_north_arrow(
    #   location = "br", which_north = "true",
    #   style = ggspatial::north_arrow_fancy_orienteering) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = gray(0.8), linetype = "dashed", size = 0.5),
      panel.background = ggplot2::element_rect(
        colour = "gray", size = 2, fill = "white"),
      legend.key.height = ggplot2::unit(1, 'cm')) 
  ggplot2::ggsave(
    plot = g, 
    filename = paste0(fig_file_name, ".png"),
    width = 20, height = 25, units = "cm", dpi = 600)
  gg <- ggiraph::girafe(
    ggobj = g + 
      ggiraph::geom_point_interactive(
        data = idw.msk.dfr,
        mapping = ggplot2::aes(
          x = x, y = y,
          tooltip = var),
        alpha = 0.01,
        show.legend = FALSE),
    width_svg = 9, 
    height_svg = 10)
  htmlwidgets::saveWidget(
    widget = gg, 
    file = paste0(fig_file_name, "_ggplot2.html"), 
    selfcontained = TRUE)
  
  ##############################################################################
  
}
################################################################################


################################################################################
# Graficar datos (escalas continuas)  #####
generar_graficos_continuos <- function() {
  
  # Se agrega el PATH al nombre del archivo, pero aún no se define la 
  # extensión, es decir, aún no se define el tipo de archivo.
  fig_file_name <- paste0(config$folders$plots, "continuous_scales/", fig_file_name)
  
  # Definir bounding box de los gráficos (tanto para leaflet como para ggplot2)
  bbox <- list(
    left = config$spatial_domain$wlo, bottom = config$spatial_domain$sla,
    right = config$spatial_domain$elo, top = config$spatial_domain$nla)
  
  ##############################################################################
  ## GRAFICOS CON LEAFLET (https://rstudio.github.io/leaflet/raster.html) ####
  
  if (data_type == "anom") {
    if (variable == 'prcp') {
      breaks <- c(-200,-100,-50,-20,-10,-5,5,10,20,50,100,200)
    } else if (variable == 't2m') {
      breaks <- c(-4,-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2,4)
    }
    grouped.idw.msk <- idw.msk
    # Los gráficos de anomalía salen muy claros!! es porque los valores
    # graficados son muy pequeños con respecto a los extremos del vector
    # breaks. La solución es ajustar el vector break a los valores 
    # de anomalía observados!
    max_value <- ceiling(max(
      abs(min(raster::values(grouped.idw.msk), na.rm = T)), 
      abs(max(raster::values(grouped.idw.msk), na.rm = T))))
    breaks <- breaks[breaks <= max_value & breaks >= -max_value]
    #
    paleta <- RColorBrewer::brewer.pal(7, 'RdBu')
    paleta <- if (variable == 't2m') rev(paleta) else paleta
  } else if (data_type == "corr") {
    breaks <- c(-1,-0.5,-0.1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
    grouped.idw.msk <- idw.msk
    red_plt  <- head(rev(RColorBrewer::brewer.pal(3, 'Reds')), 2)
    blue_plt  <- tail(grDevices::colorRampPalette(
      colors = RColorBrewer::brewer.pal(9, 'Blues'))( 11 ), 9)
    paleta <- c(red_plt, "#ffffff", blue_plt) 
  } else if (data_type == "value.gen") {  # PREV_PREC
    if (variable == 'prcp') {
      breaks <- c(0,1,25,50,100,150,200,250,300,400,500,600,700)
      paleta <- RColorBrewer::brewer.pal(9, 'Blues')
    } else if (variable == 't2m') {
      breaks <- c(-9,-6,-3,3,6,9,12,15,18,21,24,27,30,33)
      paleta <- viridis::plasma(14)
    }
    grouped.idw.msk <- idw.msk
  } else if (data_type == "value.fcst") {  
    if (variable == 'prcp') {
      breaks <- c(0,25,50,100,150,200,250,300,400,500,600,700)
      paleta <- RColorBrewer::brewer.pal(9, 'Blues')
    } else if (variable == 't2m') {
      breaks <- c(-9,-6,-3,3,6,9,12,15,18,21,24,27,30,33)
      paleta <- viridis::plasma(14)
    }
    grouped.idw.msk <- idw.msk
  } else {
    return (NULL)
  }
  
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
    # leafem::addMouseCoordinates() %>%
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
      x = idw.msk, 
      project = TRUE,
      colors = leaflet::colorNumeric(
        palette = paleta, 
        domain = breaks, 
        na.color = "transparent"),
      opacity = 0.8, 
      group = variable_fcst,
      layerId = variable_fcst)  %>%
    leafem::addImageQuery(
      x = idw.msk, 
      project = TRUE,
      type = "mousemove", 
      digits = 1, 
      prefix = "",
      group = variable_fcst,
      layerId = variable_fcst,
      position = "topright") %>%
    # leaflet::addCircles(
    #   data = idw.msk.sf) %>%
    leaflet::addLegend(
      title = variable_str,
      pal = leaflet::colorNumeric(
        palette = paleta, 
        domain = breaks, 
        na.color = "transparent"),
      values = breaks,
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
  if (data_type == "anom") {
    if (variable == 'prcp') {
      breaks <- c(-200,-100,-50,-20,-10,-5,5,10,20,50,100,200)
      grouped.idw.msk.dfr <- idw.msk.dfr
    } else if (variable == 't2m') {
      breaks <- c(-4,-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2,4)
      grouped.idw.msk.dfr <- idw.msk.dfr
    }
    # Los gráficos de anomalía salen muy claros!! es porque los valores
    # graficados son muy pequeños con respecto a los extremos del vector
    # breaks. La solución es ajustar el vector break a los valores 
    # de anomalía observados!
    max_value <- ceiling(max(
      abs(min(grouped.idw.msk.dfr$var, na.rm = T)), 
      abs(max(grouped.idw.msk.dfr$var, na.rm = T))
    ))
    breaks <- breaks[breaks <= max_value & breaks >= -max_value]
    #
    paleta <- RColorBrewer::brewer.pal(11, 'RdBu')
    paleta <- if (variable == 't2m') rev(paleta) else paleta
    labels <- breaks
  } else if (data_type == "corr") {
    breaks <- c(-1,-0.5,-0.1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
    grouped.idw.msk.dfr <- idw.msk.dfr
    red_plt  <- head(rev(RColorBrewer::brewer.pal(3, 'Reds')), 2)
    blue_plt  <- tail(grDevices::colorRampPalette(
      colors = RColorBrewer::brewer.pal(9, 'Blues'))( 11 ), 9)
    paleta <- c(red_plt, "#ffffff", blue_plt) 
    labels <- breaks
  } else if (data_type == "value.gen") {  # PREV_PREC
    if (variable == 'prcp') {
      breaks <- c(0,1,25,50,100,150,200,250,300,400,500,600,700)
      grouped.idw.msk.dfr <- idw.msk.dfr
      paleta <- RColorBrewer::brewer.pal(9, 'Blues')
    } else if (variable == 't2m') {
      breaks <- c(-9,-6,-3,0,3,6,9,12,15,18,21,24,27,30,33)
      grouped.idw.msk.dfr <- idw.msk.dfr
      paleta <- viridis::plasma(14)
    }
    labels <- breaks
  } else if (data_type == "value.fcst") {  
    if (variable == 'prcp') {
      breaks <- c(0,25,50,100,150,200,250,300,400,500,600,700)
      grouped.idw.msk.dfr <- idw.msk.dfr
      paleta <- RColorBrewer::brewer.pal(9, 'Blues')
    } else if (variable == 't2m') {
      breaks <- c(-9,-6,-3,0,3,6,9,12,15,18,21,24,27,30,33)
      grouped.idw.msk.dfr <- idw.msk.dfr
      # paleta <- grDevices::colorRampPalette(
      #   colors = RColorBrewer::brewer.pal(9, 'Oranges'))( 16 )
      # paleta <- tail(viridis::turbo(22), 16)
      paleta <- viridis::plasma(14)
    }
    labels <- breaks
  } else {
    return (NULL)
  }
  
  main_title_split <- unlist(stringr::str_split(main_title, '\n'))
  sub_title <- paste0(paste(tail(main_title_split, -1), collapse = ''))
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Generar el gráfico per se
  g <- ggplot2::ggplot(data = grouped.idw.msk.dfr,
                       width = 27, 
                       height = 30, 
                       units = "cm") +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = x, y = y,
        fill = var),
      alpha = 1) +
    # ggplot2::scale_fill_continuous(
    #   limits=c(min(breaks), max(breaks)), 
    #   breaks=breaks) +
    # ggplot2::scale_fill_distiller(
    #   type = "seq", 
    #   palette = "RdYlBu", 
    #   limits=c(min(breaks), max(breaks)), 
    #   breaks=breaks) +
    ggplot2::scale_fill_gradientn(
      colours = paleta, 
      limits = c(min(breaks), max(breaks)),
      breaks = breaks, 
      labels = labels) +
    # ggplot2::scale_fill_gradient2(
    #   low = "blue", mid = "white", high = "red",
    #   midpoint = mean(idw.msk.dfr$var)) +
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
    # ggspatial::annotation_scale(
    #   location = "bl", width_hint = 0.4) +
    # ggspatial::annotation_north_arrow(
    #   location = "br", which_north = "true",
    #   style = ggspatial::north_arrow_fancy_orienteering) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(
        color = gray(0.8), linetype = "dashed", size = 0.5),
      panel.background = ggplot2::element_rect(
        colour = "gray", size = 2, fill = "white"),
      legend.key.height = ggplot2::unit(2, 'cm')) 
  ggplot2::ggsave(
    plot = g, 
    filename = paste0(fig_file_name, ".png"),
    width = 20, height = 25, units = "cm", dpi = 600)
  gg <- ggiraph::girafe(
    ggobj = g + 
      ggiraph::geom_point_interactive(
        data = idw.msk.dfr,
        mapping = ggplot2::aes(
          x = x, y = y,
          tooltip = var),
        alpha = 0.01,
        show.legend = FALSE),
    width_svg = 9, 
    height_svg = 10)
  htmlwidgets::saveWidget(
    widget = gg, 
    file = paste0(fig_file_name, "_ggplot2.html"), 
    selfcontained = TRUE)
  
  ##############################################################################
  
}
################################################################################


################################################################################
# Graficar datos (probabilidades separadas - escala discreta - op 1)  #####
generar_graficos_prob_sep_discretos_op_1 <- function() {
  
  # Se agrega el PATH al nombre del archivo, pero aún no se define la 
  # extensión, es decir, aún no se define el tipo de archivo.
  fig_file_name <- paste0(config$folders$plots, "discrete_scales/", fig_file_name)
  
  # Definir bounding box de los gráficos (tanto para leaflet como para ggplot2)
  bbox <- list(
    left = config$spatial_domain$wlo, bottom = config$spatial_domain$sla,
    right = config$spatial_domain$elo, top = config$spatial_domain$nla)
  
  ##############################################################################
  ## GRAFICOS CON GGPLOT GGPLOT ####
  
  main_title_split <- unlist(stringr::str_split(main_title, '\n'))
  sub_title <- paste0(paste(tail(main_title_split, -1), collapse = ''))
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  breaks = c(0.33, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1)
  groups <- definir_grupos(breaks, include.lowest = T, right = T)
  labels <- c(breaks*100) %>%
    definir_grupos(include.lowest = T, right = T) %>%
    generar_etiquetas_grupos("%d")
  
  g0 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = factor(indice_grupo, levels = c(1:(length(groups)*3)))),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.menor.media') %>% 
        dplyr::select(lon, lat, var = pr.menor.media) %>%
        asignar_grupos(breaks, groups, include.lowest = T, right = T),
      alpha = 1) +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = factor(indice_grupo, levels = c(1:(length(groups)*3)))),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.media') %>% 
        dplyr::select(lon, lat, var = pr.media) %>%
        asignar_grupos(breaks, groups, include.lowest = T, right = T) %>%
        dplyr::mutate(indice_grupo = indice_grupo + length(groups)),
      alpha = 1) +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = factor(indice_grupo, levels = c(1:(length(groups)*3)))),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.mayor.media') %>% 
        dplyr::select(lon, lat, var = pr.mayor.media) %>%
        asignar_grupos(breaks, groups, include.lowest = T, right = T) %>%
        dplyr::mutate(indice_grupo = indice_grupo + length(groups)*2),
      alpha = 1) +
    ggplot2::scale_discrete_manual(
      aesthetics = "fill",
      values = c(
        if (variable == "prcp") noaa_escala_rojos else noaa_escala_azules,
        tail(RColorBrewer::brewer.pal(8, 'Greys'), 7),
        if (variable == "prcp") noaa_escala_azules else noaa_escala_rojos),
      drop = FALSE) +
    # ggplot2::scale_fill_gradientn(
    #   colours = c(
    #     tail(RColorBrewer::brewer.pal(8, if (variable == "prcp") 'YlOrBr' else 'GnBu'), 7),
    #     tail(RColorBrewer::brewer.pal(8, 'Greys'), 7),
    #     tail(RColorBrewer::brewer.pal(8, if (variable == "prcp") 'BuGn' else 'YlOrBr'), 7)),
    #   drop = FALSE) + 
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
          tooltip = pr.menor.media * 100),
        alpha = 0.01,
        show.legend = FALSE) + 
      ggiraph::geom_point_interactive(
        data = idw.pr.dfr %>% 
          dplyr::filter(pr.max.col == 'pr.media'),
        mapping = ggplot2::aes(
          x = lon, y = lat,
          tooltip = pr.media * 100),
        alpha = 0.01,
        show.legend = FALSE) + 
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
  htmlwidgets::saveWidget(
    widget = gg0, 
    file = paste0(fig_file_name, "_ggplot2.html"), 
    selfcontained = TRUE)
  
  g1 <- ggplot2::ggplot(width = 27, height = 30, units = "cm") +
    ggplot2::geom_raster(
      mapping = ggplot2::aes(
        x = lon, y = lat,
        fill = factor(indice_grupo, levels = c(1:(length(groups))))),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.menor.media') %>% 
        dplyr::select(lon, lat, var = pr.menor.media) %>%
        asignar_grupos(breaks, groups, include.lowest = T, right = T),
      alpha = 1) +
    ggplot2::scale_discrete_manual(
      aesthetics = "fill",
      breaks = c(1:length(groups)),
      values = if (variable == "prcp") noaa_escala_rojos else noaa_escala_azules,
      labels = labels,
      drop = FALSE) +
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
        data = idw.pr.dfr %>% dplyr::filter(pr.max.col == 'pr.menor.media'),
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
        fill = factor(indice_grupo, levels = c(1:(length(groups))))),
      data =idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.media') %>% 
        dplyr::select(lon, lat, var = pr.media) %>%
        asignar_grupos(breaks, groups, include.lowest = T, right = T),
      alpha = 1) +
    ggplot2::scale_discrete_manual(
      aesthetics = "fill",
      breaks = c(1:length(groups)),
      values = tail(RColorBrewer::brewer.pal(8, 'Greys'), 7),
      labels = labels,
      drop = FALSE) +
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
        data = idw.pr.dfr %>% dplyr::filter(pr.max.col == 'pr.media'),
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
        fill = factor(indice_grupo, levels = c(1:(length(groups))))),
      data = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == 'pr.mayor.media') %>% 
        dplyr::select(lon, lat, var = pr.mayor.media) %>%
        asignar_grupos(breaks, groups, include.lowest = T, right = T),
      alpha = 1) +
    ggplot2::scale_discrete_manual(
      aesthetics = "fill",
      breaks = c(1:length(groups)),
      values = if (variable == "prcp") noaa_escala_azules else noaa_escala_rojos,
      labels = labels,
      drop = FALSE) +
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
        data = idw.pr.dfr %>% dplyr::filter(pr.max.col == 'pr.mayor.media'),
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
# Graficar datos (probabilidades separadas - escala discreta - op 2)  #####
generar_graficos_prob_sep_discretos_op_2 <- function() {
  
  # Se agrega el PATH al nombre del archivo, pero aún no se define la 
  # extensión, es decir, aún no se define el tipo de archivo.
  fig_file_name <- paste0(config$folders$plots, "discrete_scales/", fig_file_name)
  
  # Definir bounding box de los gráficos (tanto para leaflet como para ggplot2)
  bbox <- list(
    left = config$spatial_domain$wlo, bottom = config$spatial_domain$sla,
    right = config$spatial_domain$elo, top = config$spatial_domain$nla)
  
  ##############################################################################
  ## GRAFICOS CON LEAFLET (https://rstudio.github.io/leaflet/raster.html) ####
  
  # Se definen y asignan los grupos para las probabilidades menor a la media
  breaks.menor.media <- 
    c(133,140,150,160,170,180,190,200)
  groups.menor.media <- 
    definir_grupos(breaks.menor.media, include.lowest = T)
  grouped.menor.media <- 
    asignar_grupos(
      dfr = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == "pr.menor.media") %>% 
        dplyr::mutate(var = pr),
      breaks = breaks.menor.media, groups = groups.menor.media, 
      include.lowest = T) %>%
    dplyr::mutate(
      indice_grupo = indice_grupo + 0) %>%
    dplyr::select(
      lon, lat, grupo, indice_grupo)
  
  # Se definen y asignan los grupos para las probabilidades iguales a la media
  breaks.media <- 
    c(233,240,250,260,270,280,290,300)
  groups.media <- 
    definir_grupos(breaks.media, include.lowest = T)
  grouped.media <- 
    asignar_grupos(
      dfr = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == "pr.media") %>% 
        dplyr::mutate(var = pr),
      breaks = breaks.media, groups = groups.media, 
      include.lowest = T) %>%
    dplyr::mutate(
      indice_grupo = indice_grupo + 7) %>%
    dplyr::select(
      lon, lat, grupo, indice_grupo)
  
  # Se definen y asignan los grupos para las probabilidades mayores a la media
  breaks.mayor.media <- 
    c(333,340,350,360,370,380,390,300)
  groups.mayor.media <- 
    definir_grupos(breaks.mayor.media, include.lowest = T)
  grouped.mayor.media <- 
    asignar_grupos(
      dfr = idw.pr.dfr %>% 
        dplyr::filter(pr.max.col == "pr.mayor.media") %>% 
        dplyr::mutate(var = pr),
      breaks = breaks.mayor.media, groups = groups.mayor.media, 
      include.lowest = T) %>%
    dplyr::mutate(
      indice_grupo = indice_grupo + 14) %>%
    dplyr::select(
      lon, lat, grupo, indice_grupo)
  
  # Se unen en un solo dataframe todos los grupos asignados
  # Esto es necesario para poder agregar estos datos a grouped.idw.msk.dfr
  grouped.all <- grouped.menor.media %>%
    dplyr::bind_rows(grouped.media) %>%
    dplyr::bind_rows(grouped.mayor.media)
  
  
  # Se ...
  grouped.idw.msk.dfr <- idw.pr.dfr %>%
    dplyr::left_join(grouped.all, by = c("lon", "lat"))
  
  
  labels_breaks <- c(33, 40, 50, 60, 70, 80, 90, 100)
  labels_groups <- definir_grupos(labels_breaks, include.lowest = T)
  labels <- generar_etiquetas_grupos(labels_groups, "%d")
  
  
  idw.pr.raster <-
    raster::rasterFromXYZ(
      xyz = grouped.idw.msk.dfr %>%
        dplyr::select(x = lon, y = lat, var = indice_grupo)) %>%
    raster::crop(crcsas_sp) %>%
    raster::mask(crcsas_sp)
  raster::crs(idw.pr.raster) <- "EPSG:4326"
  
  idw.pr.raster.obs <-
    raster::rasterFromXYZ(
      xyz = grouped.idw.msk.dfr %>%
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
          if (variable == "prcp") noaa_escala_rojos else noaa_escala_azules,
          tail(RColorBrewer::brewer.pal(8, 'Greys'), 7),
          if (variable == "prcp") noaa_escala_azules else noaa_escala_rojos),
        domain = c(1:21),
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
      colors = if (variable == "prcp") noaa_escala_rojos else noaa_escala_azules,
      labels = labels,
      position = "bottomright") %>%
    leaflet::addLegend(
      title = "Near Normal (%)",
      colors = tail(RColorBrewer::brewer.pal(8, 'Greys'), 7),
      labels = labels,
      position = "bottomright") %>%
    leaflet::addLegend(
      title = "Below Normal (%)",
      colors = if (variable == "prcp") noaa_escala_azules else noaa_escala_rojos,
      labels = labels,
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

