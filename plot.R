
# ---
# --- Creación de gráficos para las salidas del CPT
# ---

# -----------------------------------------------------------------------------#
# --- PASO 1. Inicializar entorno ----

# i. Borrar objetos existentes en el ambiente
rm(list = ls()); gc()

# ii. Configurar huso horario en UTC
Sys.setenv(TZ = "UTC")

# iii. Cargar paquetes a utilizar
list.of.packages <- c("dplyr")
for (pack in list.of.packages) {
  if (! require(pack, character.only = TRUE)) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}

# iv. Verificar si están instalados los paquetes necesarios
list.of.packages <- c("stringr", "tidyr", "yaml", "glue")
for (pack in list.of.packages) {
  if(pack %in% rownames(installed.packages()) == FALSE) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}

rm(pack, list.of.packages); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 2. Leer archivos YML de configuracion y parametros----

# i. Leer YML de configuracion
args <- base::commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  archivo.config <- args[1]
} else {
  # No vino el archivo de configuracion por linea de comandos.
  # Utilizo un archivo default
  archivo.config <- paste0(getwd(), "/plot.yaml")
}

if (! file.exists(archivo.config)) {
  stop(paste0("El archivo de configuracion de ", archivo.config, " no existe\n"))
} else {
  cat(paste0("Leyendo archivo de configuracion ", archivo.config, "...\n"))
  config <- yaml::yaml.load_file(archivo.config)
}

rm(archivo.config, args); gc()
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 3. Cargar librerias propias e iniciar script ----

# a) Configurar reticulate para poder utilizar código python
# py_venv <- paste0(getwd(), "/venv")
# reticulate::use_virtualenv(py_venv, required=TRUE)

# b) Carga de clases de uso general 
# reticulate::source_python("components.py")

# c) Carga de codigo para controles de calidad
# source(paste0(config$dir$base, "lib/", "funciones_control_calidad.R"), echo = FALSE)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Leer archivos de salida del CPT y generar gráficos----



for (fp in config$files) {

  fp <- config$files[[1]]
  
  
  #
  # PARSEAR NOMBRE DEL ARCHIVO
  #
  
  fp_split <- stringr::str_split_fixed(fp$file, '_', 6)
  
  modelo <- fp_split[1]
  variable <- stringr::str_split_fixed(fp_split[2], '-', 2)[2]
  forecast_month <- as.numeric(fp_split[4])
  initial_month <- stringr::str_replace(fp_split[3], 'ic', '')
  training_period <- fp_split[5]
  first_training_year <- as.numeric(stringr::str_split_fixed(training_period, '-', 2)[1])
  last_training_year <- as.numeric(stringr::str_split_fixed(training_period, '-', 2)[2])
  forecast_years <- stringr::str_replace(fp_split[6], '.txt', '')
  first_fcst_year <- as.numeric(stringr::str_split_fixed(forecast_years, '-', 2)[1])
  last_fcst_year <- as.numeric(stringr::str_split_fixed(forecast_years, '-', 2)[2])
  
  
  
  #
  # EXTRAER DATOS GENERADOS POR EL SOFTWARE CPT
  #
  
  # Definir path absoluto al archivo
  file_abs_path <- paste0(getwd(), '/', config$folders$generated_data, fp$file)
  # Extraer latitudes (usar nombre de columna como ID)
  y <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 3, nrows = 1)
  yy <- y %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lat") %>% 
    dplyr::select(-V1)
  # Extraer longitudes (usar nombre de columna como ID)
  x <- read.table(file =file_abs_path, sep = '\t', header = FALSE, skip = 4, nrows = 1)
  xx <- x %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lon") %>% 
    dplyr::select(-V1)
  # Extraer valores (usar año y columna como ID)
  c <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 5)
  cc <- c %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = 'value') %>% 
    dplyr::rename(year = V1) %>%
    dplyr::mutate(value = ifelse(value == -999, NA, value)) 
     
  # Unir los datos extraídos en único dataframe largo (es más facil hacer calculos estadísticos así, con groupby)
  gen_data <- dplyr::left_join(yy, xx, by = 'columna') %>% 
    dplyr::left_join(cc, by = 'columna') %>% 
    dplyr::select(-columna)
  
  # Remover objetos que ya no se van a utilizar
  rm(file_abs_path, y, yy, x, xx, c, cc); gc()
  
  
  
  #
  # EXTRAER DATOS OBSERVADOS
  #
  
  # Definir path absoluto al archivo
  obs_file <- paste0(variable, '_', forecast_month, '.txt')
  file_abs_path <- paste0(getwd(), '/', config$folders$observed_data, obs_file)
  # Extraer latitudes (usar nombre de columna como ID)
  y <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 1, nrows = 1)
  yy <- y %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lat") %>% 
    dplyr::select(-V1)
  # Extraer longitudes (usar nombre de columna como ID)
  x <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 2, nrows = 1)
  xx <- x %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lon") %>% 
    dplyr::select(-V1)
  # Extraer valores (usar año y columna como ID)
  c <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 5)
  cc <- c %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = 'value') %>% 
    dplyr::rename(year = V1) %>%
    dplyr::mutate(value = ifelse(value == -999, NA, value)) 
  
  # Unir los datos extraídos en único dataframe largo (es más facil hacer calculos estadísticos así, con groupby)
  obs_data <- dplyr::left_join(yy, xx, by = 'columna') %>% 
    dplyr::left_join(cc, by = 'columna') %>% 
    dplyr::select(-columna)
  
  # Remover objetos que ya no se van a utilizar
  rm(obs_file, file_abs_path, y, yy, x, xx, c, cc); gc()
  
  
  
  #
  # CORREGIR RESULTADOS CON VALORES MAYORES A 6 DESV ESTÁNDAR
  #
  
  # Calcular estadísticas sobre de los datos observados
  sd_obs_data <- obs_data %>%
    dplyr::group_by(lat, lon) %>%
    dplyr::summarise(
      sd = sd(value, na.rm = TRUE) # desviación estándar, excel pt: DESVPAD
    )
  
  # Corregir valores fuera de rango (asignar random entre 0 y 0.1, sin 0 y 0.1)
  # el problema es que la correlación lanza warnings si reemplazo todos por 0!!
  gen_data_corregido <- gen_data %>%
    dplyr::left_join(sd_obs_data, by = c('lat', 'lon')) %>%
    dplyr::mutate(aux_value = runif(n = n(), min = 0, max = 0.1)) %>%
    dplyr::mutate(value = ifelse(value >= 7*sd, aux_value, value)) %>%
    dplyr::select(-sd, -aux_value)
  
  # Control
  control_correccion <- gen_data_corregido %>%
    dplyr::left_join(gen_data, by = c('lat', 'lon', 'year'), suffix = c('.o', '.c')) %>%
    dplyr::filter(value.o != value.c) %>%
    dplyr::left_join(sd_obs_data, by = c('lat', 'lon'))
  
  # Reemplazar gen_data por gen_data_corregido
  gen_data <- gen_data_corregido
  
  # Remover objetos que ya no se van a utilizar
  rm(sd_obs_data, gen_data_corregido); gc()
  
  
  
  #
  # AGREGAR COLUMNAS NECESARIAS PARA REALIZAR LOS GRÁFICOS
  #
  
  # R redondea .5 para abajo, excel para arriba entonces, 
  # se crea una función que redondee igual que excel!!
  round2 = function(x, n) {
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5 + sqrt(.Machine$double.eps)
    z = trunc(z)
    z = z/10^n
    z*posneg
  }
  
  # Calcular media, varianza, correlación, anomalias y prev_norm 
  # entre los datos observados y generados por CPT
  data <- gen_data %>%
    dplyr::left_join(obs_data, by = c('lat', 'lon', 'year'), suffix = c('.gen', '.obs')) %>%
    dplyr::group_by(lat, lon) %>%
    dplyr::mutate(
      mean = mean(value.obs, na.rm = TRUE), # media, excel pt: AVERAGE
      var = var(value.obs, na.rm = TRUE), # varianza, excel pt: VAR
      corr = cor(value.gen, value.obs, use = 'na.or.complete'), # correlation, excel pt: CORREL
      prev_norm = (value.gen - mean) / sqrt(var),
      anom = value.gen - mean
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      dp = 1 - corr^2  # distancia de la previsión en relación a la distribución observada
    ) %>%
    dplyr::mutate(
      pr.menor.media = round2(stats::pnorm(-0.4303, prev_norm, dp), 2),
      pr.menor.media.round = round2(pr.menor.media, 1),
      pr.menor.media.diff = pr.menor.media - pr.menor.media.round,
      pr.menor.media.ajuste = dplyr::case_when(
        pr.menor.media.diff >= 0.025 ~ 0.05,
        pr.menor.media.diff > -0.025 & pr.menor.media.diff < 0.025 ~ 0,
        pr.menor.media.diff <= -0.025 ~ -0.05
      ),
      pr.menor.media.ajustado = pr.menor.media - pr.menor.media.diff + pr.menor.media.ajuste
    ) %>% 
    dplyr::mutate(
      pr.media = round2(stats::pnorm(0.4303, prev_norm, dp) - pr.menor.media, 2),
      pr.media.round = round2(pr.menor.media, 1),
      pr.media.diff = pr.menor.media - pr.menor.media.round,
      pr.media.ajuste = dplyr::case_when(
        pr.media.diff >= 0.025 ~ 0.05,
        pr.media.diff > -0.025 & pr.media.diff < 0.025 ~ 0,
        pr.media.diff <= -0.025 ~ -0.05
      ),
      pr.media.ajustado = pr.media - pr.media.diff + pr.media.ajuste
    ) %>% 
    dplyr::mutate(
      pr.mayor.media = 1 - pr.menor.media - pr.media,
      pr.mayor.media.ajustado = 1 - pr.menor.media.ajustado - pr.media.ajustado
    ) %>%
    dplyr::mutate(
      escala_abajo = ifelse(pr.menor.media.ajustado > pr.mayor.media.ajustado,
                            pr.menor.media.ajustado, NA),
      escala_arriba = ifelse(pr.mayor.media.ajustado >= pr.menor.media.ajustado,
                             pr.mayor.media.ajustado, NA)
    ) %>% 
    dplyr::mutate(
      codigo_de_color = dplyr::case_when(
        pr.menor.media.ajustado > pr.mayor.media.ajustado & pr.menor.media.ajustado >= 0.6 ~ -6,
        pr.menor.media.ajustado > pr.mayor.media.ajustado & pr.menor.media.ajustado >= 0.5 & pr.menor.media.ajustado < 0.6 ~ -5,
        pr.menor.media.ajustado > pr.mayor.media.ajustado & pr.menor.media.ajustado >= 0.45 & pr.menor.media.ajustado < 0.5 ~ -4,
        pr.menor.media.ajustado > pr.mayor.media.ajustado & pr.menor.media.ajustado >= 0.4 & pr.menor.media.ajustado < 0.45 ~ -3,
        pr.menor.media.ajustado > pr.mayor.media.ajustado & pr.menor.media.ajustado >= 0.35 & pr.menor.media.ajustado < 0.4 ~ -2,
        pr.menor.media.ajustado > pr.mayor.media.ajustado & pr.menor.media.ajustado >= 0 & pr.menor.media.ajustado < 0.35 ~ -1,
        pr.mayor.media.ajustado >= pr.menor.media.ajustado & pr.mayor.media.ajustado >= 0 & pr.mayor.media.ajustado < 0.35 ~ 1,
        pr.mayor.media.ajustado >= pr.menor.media.ajustado & pr.mayor.media.ajustado >= 0.35 & pr.mayor.media.ajustado < 0.4 ~ 2,
        pr.mayor.media.ajustado >= pr.menor.media.ajustado & pr.mayor.media.ajustado >= 0.4 & pr.mayor.media.ajustado < 0.45 ~ 3,
        pr.mayor.media.ajustado >= pr.menor.media.ajustado & pr.mayor.media.ajustado >= 0.45 & pr.mayor.media.ajustado < 0.5 ~ 4,
        pr.mayor.media.ajustado >= pr.menor.media.ajustado & pr.mayor.media.ajustado >= 0.5 & pr.mayor.media.ajustado < 0.6 ~ 5,
        pr.mayor.media.ajustado >= pr.menor.media.ajustado & pr.mayor.media.ajustado >= 0.6 ~ 6
      )
    )
  
  
  #
  # SELECCIONAR AÑOS PRONOSTICADOS Y RENOMBRARLOS
  #
  
  # Ocurre que cuando se corre el CPT, algunas veces el año siguiente al último
  # año de entrenamiento no es el año que dice ser sino el 1er año pronosticado.
  # Para saber si se dió esta situación se debe verificar que el año inmediata-
  # mente posterior al último año de entrenamiento sea igual al año de pronos-
  # tico (forecast_data.fyr en el archivo config.yaml). OJO: para corridas con
  # datos del modelo europeo este truco con los años no aplica.
  
  
  # Verificar periodo de entrenamiento
  if (first_training_year != min(gen_data$year)) {
    stop(glue::glue('Wrong training period for file {fp$file}. First training',
                    'period year mismatch with the min year in generated data'))
  }
  if (!last_training_year %in% unique(data$year)) {
    stop(glue::glue('Wrong training period for file {fp$file}. Last training',
                    'period year are not present in generated data'))
  }
  
  
  # Calcular desfasaje
  min_date_in_data <- gen_data %>% 
    dplyr::filter(year > last_training_year) %>%
    dplyr::pull(year) %>% min()
    
  desfasaje <- first_fcst_year - min_date_in_data
  
  
  # Renombrar los años segun corresponda y descartar los años de entrenamiento
  mapeo <- purrr::map_dfr(
    .x = gen_data %>% 
      dplyr::filter(year > last_training_year) %>%
      dplyr::pull(year) %>% unique(),
    .f = function(year) {
      tibble::tibble(orig_year = year, new_year = year + desfasaje)
    })
  data <- data %>%
    dplyr::filter(year > last_training_year) %>%
    dplyr::left_join(mapeo, by = c('year' = 'orig_year')) %>%
    dplyr::mutate(new_year = ifelse(is.na(new_year), year, new_year)) %>%
    dplyr::mutate(year = new_year) %>%
    dplyr::select(-new_year) %>%
    dplyr::arrange(year)
  
  
  
  #
  # GENERAR GRÁFICOS/MAPAS
  #
  
  for (data_year in unique(data$year)) {
    for (data_type in c("anom", "corr", "value.gen", "codigo_de_color")) {
    
      #DADOS=read.table("TXT/ANOM_PREC_NMME_2020.txt",header = T,sep="")
      datos <- data %>% dplyr::filter(year == data_year) %>% 
        dplyr::select(lon, lat, var = !!data_type) %>% 
        dplyr::filter(!is.na(var))
      #COMP <- DADOS %>% dplyr::left_join(DATOS, by = c("Lon", "Lat"))
      ############################################################################################
      ## Gerando uma grade regular ##################
      resolucao=0.5
      x.range <- as.numeric(c(min(gen_data$lon), max(gen_data$lon)))  # min/max longitude of the interpolation area
      y.range <- as.numeric(c(min(gen_data$lat), max(gen_data$lat)))  
      grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resolucao), 
                         y = seq(from = y.range[1], to = y.range[2], by = resolucao))  # expand points to grid
      sp::coordinates(grd) <- ~x + y
      sp::gridded(grd) <- TRUE
      sp::coordinates(datos) = ~lon + lat  ## Convertendo data.frame para SpatialPointsData.frame
      idw <- gstat::idw(formula = var ~ 1, locations = datos, newdata = grd)  #
      idw.output = as.data.frame(idw)  # Convertendo para um data.frame
      names(idw.output) <- c("long", "lat", "var") 
      idw.r <- rasterFromXYZ(idw.output[,1:3])
      crcsas <- sf::as_Spatial(sf::st_read(paste0(config$folders$shapefiles, "/CRC_SAS.shp")))
      idw.crp <- crop(idw.r, crcsas)
      idw.msk <- mask(idw.crp, crcsas)
      idw.msk.dfr <- as.data.frame(rasterToPoints(idw.msk))
      if (data_type == "anom") {
        seqq <- c(-200,-100,-50,-20,-10,-5,5,10,20,50,100,200)
        seq.l <- c(-200,-100,-50,-20,-10,-5,5,10,20,50,100,200)
        main_title <- glue::glue("Precipitation Anomaly Forecast (mm)\nValid for ",
                                 "{month.name[forecast_month]} {data_year} - ",
                                 "{toupper(modelo)}\nIssued: {initial_month} ",
                                 "{data_year}")
        paleta <- c("#fe0000","#ff6766","#ff6766","#ff9899","#fecccb","#fecccb","#ffffff",
                    "#ffffff","#ccccfe","#9a99ff","#6599ff","#6665fe","#0000fe","#0000fe")
        teste <- grDevices::colorRampPalette(paleta)
        par_settings <- rasterVis::rasterTheme(region=teste(14))
        col_regions  <- NULL
        fig_file_name <- paste0(
          config$folders$output, 
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_anom.jpg')
      } else if (data_type == "corr") {
        seqq <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
        seq.l <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
        main_title <- glue::glue("Correlation between Forecast and Observation ",
                                 "(1982-2010)\nValid for {month.name[forecast_month]} - ",
                                 "{toupper(modelo)}\nIssued: {initial_month}")
        paleta <- c("#ff6766","#ff9899","#fecccb","#99ffff","#00ffff","#00ccff","#99cccd",
                    "#6599ff","#6665fe","#0000fe")
        teste=colorRampPalette(paleta)
        par_settings <- rasterVis::rasterTheme(region=teste(11))
        col_regions  <- NULL
        fig_file_name <- paste0(
          config$folders$output, 
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_corr.jpg')
      } else if (data_type == "value.gen") {  # PREV_PREC¨
        seqq <- c(0,1,25,50,100,150,200,250,300,400,500)
        seq.l <- c(0,1,25,50,100,150,200,250,300,400,500)
        main_title <- glue::glue("Precipitation (mm)\nValid for {month.name[forecast_month]} ",
                                 "{data_year} - {toupper(modelo)}\nIssued: {initial_month} ",
                                 "{data_year}")
        paleta <- c("#ff6634","#ff9934","#ffcc00","#ffffcd","#cdffcc","#9acc99","#34cc67",
                    "#33cc33","#019934","#006634")
        teste=grDevices::colorRampPalette(paleta)
        par_settings <- rasterVis::rasterTheme(region=teste(10))
        col_regions  <- NULL
        fig_file_name <- paste0(
          config$folders$output, 
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_prev.jpg')
      } else if (data_type == "codigo_de_color") {  # PROB_PREC
        seqq <- c(-6,-5,-4,-3,-2,-1,1,2,3,4,5,6)
        seq.l <- c("70","60","50","45","40","35","35","40","45","50","60","70")
        main_title <- glue::glue("Precipitation - Probability Forecast\nValid for ",
                                 "{month.name[forecast_month]} {data_year} - ",
                                 "{toupper(modelo)}\nIssued: {initial_month} ",
                                 "{data_year}")
        paleta <- c("#783301","#a9461d","#cf8033","#e8b832","#fafb01",
                    "#c8c8c8","#c5ffe7","#96feaf","#67ff78","#35e43f","#06c408")
        par_settings <- list(prob.b, layout.heights = list(xlab.key.padding=2.5))
        col_regions <- prob.b(11)
        fig_file_name <- paste0(
          config$folders$output, 
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_prob.jpg')
      }
      seq.interval <- seq(1, length(seqq), by=1)
      myColorkey <- list(
        at = seq.interval, 
        labels = list(
          at = seq.interval, 
          labels = as.character(seq.l)), 
        space = "bottom",
        width = 1)
      pais <- as(sf::st_read(paste0(config$folders$shapefiles, "/10m_admin_0_countries.shp")), 'Spatial')
      
      BELOW <- lattice::levelplot(x = var ~ x * y, 
                                  data = idw.msk.dfr,
                                  contour = F,
                                  at = seqq,
                                  colorkey = myColorkey,
                                  par.settings = par_settings,
                                  col.regions = col_regions,
                                  main = main_title,
                                  xlab = NULL, ylab = NULL) +
        latticeExtra::layer(sp.lines(pais, alpha=1))
      grDevices::jpeg(filename = fig_file_name, width = 16, height = 21, 
                      quality = 75, units = "cm", res = 300)
      print(BELOW)
      if (data_type == "codigo_de_color") {
        grid::grid.text('Near', rot=0, y=unit(0.09, "npc"), 
                        x=unit(0.512, "npc"))
        grid::grid.text('Below', rot=0, y=unit(0.09, "npc"), 
                        x=unit(0.28, "npc"))
        grid::grid.text('Above', rot=0, y=unit(0.09, "npc"), 
                        x=unit(0.75, "npc"))
      }
      grDevices::dev.off()
      
    }
  }

}
# ------------------------------------------------------------------------------
