
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
source("funciones.R", echo = FALSE)
# source(paste0(config$dir$base, "lib/", "funciones_control_calidad.R"), echo = FALSE)
# ------------------------------------------------------------------------------

# -----------------------------------------------------------------------------#
# --- PASO 4. Leer archivos de salida del CPT y generar gráficos----

# Leer shapes a ser utlizados en los gráficos
crcsas_sf <- sf::st_read(paste0(config$folders$shapefiles, "/CRC_SAS.shp"))
crcsas_sp <- sf::as_Spatial(crcsas_sf)
paises_sf <- sf::st_read(paste0(config$folders$shapefiles, "/10m_admin_0_countries.shp"))
paises_sp <- as(paises_sf, 'Spatial')

# Procesar archivos de salida del CPT 
for (fp in config$files) {
  cat("\n\n\n") 
  print(glue::glue("Processing file: {fp$file}"))

  #
  # PARSEAR NOMBRE DEL ARCHIVO
  #
  
  # fp <- config$files[[1]]
  # fp$file <- "nmme_precip-prcp_Mayic_6_1982-2010_2020-2021.txt"

  fp_split <- stringr::str_split_fixed(stringr::str_replace(fp$file, '.txt', ''), '_', 6)
  
  modelo <- fp_split[1]
  variable <- stringr::str_split_fixed(fp_split[2], '-', 2)[2]
  variable_str <- ifelse(variable == 'prcp', 'Precipitation', 'Average Air Temperature - 2m')
  variable_unit <- ifelse(variable == 'prcp', 'mm', '°C')
  variable_fcst <- ifelse(variable == 'prcp', 'precip', 'tmp2m')
  initial_month <- stringr::str_replace(fp_split[3], 'ic', '')
  initial_month_int <- which(initial_month == month.abb)[[1]]
  forecast_months <- fp_split[4]
  first_fcst_month <- as.numeric(stringr::str_split_fixed(forecast_months, '-', 2)[1])
  last_fcst_month <- as.numeric(stringr::str_split_fixed(forecast_months, '-', 2)[2])
  forecast_months_str <- ifelse(
    fp$type == "monthly", month.name[first_fcst_month], 
    paste0(month.abb[first_fcst_month], '-', month.abb[last_fcst_month]))
  forecast_months_abb <- ifelse(
    fp$type == "monthly", month.abb[first_fcst_month], 
    paste0(month.abb[first_fcst_month], '-', month.abb[last_fcst_month]))
  training_period <- fp_split[5]
  first_training_year <- as.numeric(stringr::str_split_fixed(training_period, '-', 2)[1])
  last_training_year <- as.numeric(stringr::str_split_fixed(training_period, '-', 2)[2])
  forecast_years <- fp_split[6]
  first_fcst_year <- as.numeric(stringr::str_split_fixed(forecast_years, '-', 2)[1])
  last_fcst_year <- as.numeric(stringr::str_split_fixed(forecast_years, '-', 2)[2])
  
  # fp$file <- "nmme_precip-prcp_Mayic_6_1982-2010_2020-2021_fabricio.txt"
  
  #
  # EXTRAER DATOS DEL ARCHIVO FORECAST SIN CALIBRAR
  #
  
  # Definir path absoluto al archivo
  if (modelo %in% c('ecmwf')) {
    
    forecast_file <- glue::glue("{modelo}_{variable_fcst}_{initial_month}ic_",
                                "{forecast_months}_{training_period}_",
                                "{forecast_years}.txt")
    file_abs_path <- paste0(getwd(), '/', config$folders$predictors, forecast_file)
    # Extraer latitudes (usar nombre de columna como ID)
    x <- read.table(file =file_abs_path, sep = '\t', header = FALSE, skip = 5, nrows = 1)
    xx <- x %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lon") %>% 
      dplyr::select(-V1)
    # Extraer valores (usar año y columna como ID)
    cc <- purrr::map2_dfr(
      .x = c(first_training_year:last_training_year, first_fcst_year:last_fcst_year),
      .y = seq(from = 6, 
               length.out = length(c(first_training_year:last_training_year, first_fcst_year:last_fcst_year)), 
               by = length(c(config$spatial_domain$nla:config$spatial_domain$sla))+2),
      .f = function(year, skip) {
        c <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = skip, 
                        nrows = length(c(config$spatial_domain$nla:config$spatial_domain$sla)))
        cc <- c %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = 'value') %>% 
          dplyr::rename(lat = V1) %>% dplyr::mutate(year = year) %>%
          dplyr::mutate(value = ifelse(value == -999, NA, value)) %>%
          dplyr::select(columna, lat, year, dplyr::everything())
      }
    )
    
    # Unir los datos extraídos en único dataframe largo (es más facil hacer calculos estadísticos así, con groupby)
    n_days <- ifelse(
      fp$type == "monthly", lubridate::days_in_month(first_fcst_month), 
      sum(lubridate::days_in_month(first_fcst_month:last_fcst_month)))
    
    fcst_data <- dplyr::left_join(xx, cc, by = 'columna') %>% 
      dplyr::select(-columna) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(value = ifelse(variable == 'prcp', value * n_days, value)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(year >= first_fcst_year)
    
    # Si los datos descargados están en grados Kelvin, pasarlos a grados Celsius
    # Algunos archivos fueron descargados en grados Kelvin! Se agrega este control
    # por si se llega a leer alguno de ellos en algún momento!
    if (mean(fcst_data$value) > 200 && variable == 't2m')
      fcst_data <- fcst_data %>% 
        dplyr::mutate(value = value - 273.15) 
    
    # Remover objetos que ya no se van a utilizar
    rm(forecast_file, file_abs_path, x, xx, cc, n_days); gc()
    
  } else {
    
    fcst_data <- purrr::map_dfr(
      .x = c(first_fcst_year:last_fcst_year),
      .f = function(year) {
        
        year_str <- ifelse(fp$type == "monthly", year, paste0(year, '-', 
                           ifelse(first_fcst_month < last_fcst_month, year, year+1)))
        forecast_file <- glue::glue("{modelo}_{variable_fcst}_fcst_{initial_month}ic_",
                                    "{forecast_months}_{year_str}.txt")
        file_abs_path <- paste0(getwd(), '/', config$folders$forecasts, forecast_file)
        # Extraer latitudes (usar nombre de columna como ID)
        x <- read.table(file =file_abs_path, sep = '\t', header = FALSE, skip = 3, nrows = 1)
        xx <- x %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lon") %>% 
          dplyr::select(-V1)
        # Extraer valores (usar año y columna como ID)
        c <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 4, nrows = 181)
        cc <- c %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = 'value') %>% 
          dplyr::rename(lat = V1) %>% dplyr::mutate(year = year) %>%
          dplyr::mutate(value = ifelse(value == -999, NA, value)) %>%
          dplyr::select(columna, lat, year, dplyr::everything())
        
        # Unir los datos extraídos en único dataframe largo (es más facil hacer calculos estadísticos así, con groupby)
        n_days <- ifelse(
          fp$type == "monthly", lubridate::days_in_month(first_fcst_month), 
          sum(lubridate::days_in_month(first_fcst_month:last_fcst_month)))
        
        fcst_data <- dplyr::left_join(xx, cc, by = 'columna') %>% 
          dplyr::select(-columna) %>% 
          dplyr::rowwise() %>%
          dplyr::mutate(value = ifelse(variable == 'prcp', value * n_days, value)) %>%
          dplyr::ungroup() 
      }
    )
    
  }
  
  
  
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
  
  # obs_file <- 'prcp_6_fabricio.txt'
  
  # Definir path absoluto al archivo
  obs_file <- paste0(variable, '_', forecast_months, '.txt') 
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
  c <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 3)
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
  # CORREGIR RESULTADOS CON VALORES MAYORES A 7 DESV ESTÁNDAR (SOLO PRCP)
  #
  
  if (variable == "prcp") {
    
    # Calcular estadísticas sobre de los datos observados
    sd_obs_data <- obs_data %>%
      dplyr::group_by(lat, lon) %>%
      dplyr::mutate(
        mx = ifelse(any(!is.na(value)), max(value, na.rm = TRUE), NA),
        point_to_check = sum(value, na.rm = T) <= 0.2,
        sd = sd(value, na.rm = TRUE) # desviación estándar, excel pt: DESVPAD
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        min_valid_value = mx - sd*7,
        max_valid_value = mx + sd*7
      ) %>%
      dplyr::mutate(
        min_valid_value = ifelse(min_valid_value >= 0, min_valid_value, 0)
      ) %>%
      dplyr::select(-mx, -sd)
    
    # Corregir valores fuera de rango (asignar random entre 0 y 0.1, sin 0 y 0.1)
    # el problema es que la correlación lanza warnings si reemplazo todos por 0!!
    gen_data_corregido <- gen_data %>%
      dplyr::left_join(sd_obs_data, by = c('lat', 'lon', 'year'), suffix = c(".gen", ".obs")) %>%
      dplyr::mutate(
        aux_value = runif(n = n(), min = 0, max = 0.1)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        in_range = dplyr::between(value.gen, min_valid_value, max_valid_value)) %>%
      dplyr::mutate(
        value.new = ifelse(!is.na(in_range) & !in_range, aux_value, value.gen)
      ) %>%
      dplyr::ungroup() 
    
    # Control
    control_correccion <- gen_data_corregido %>% 
      dplyr::filter(value.gen != value.new)
    
    # Reemplazar gen_data por gen_data_corregido
    if (nrow(control_correccion) > 0) {
      gen_data <- gen_data_corregido %>% 
        dplyr::select(lat, lon, year, value = value.new)
    }
    
    # Remover objetos que ya no se van a utilizar
    rm(sd_obs_data, gen_data_corregido); gc()
  
  }
  
  
  #
  # AGREGAR COLUMNAS NECESARIAS PARA REALIZAR LOS GRÁFICOS
  #
  
  # R redondea .5 para abajo, excel para arriba entonces, 
  # se crea una función que redondee igual que excel!!
  round2 <- function(x, n) {
    posneg <- sign(x)
    z <- abs(x)*10^n
    z <- z + 0.5 + sqrt(.Machine$double.eps)
    z <- trunc(z)
    z <- z/10^n
    z*posneg
  }
  
  # Calcular media, varianza, correlación. OJO: la media, la varianza y la 
  # correlación se calculan utilizando solo los datos observados y generados 
  # en el periodo de entrenamiento!!!
  data_obs_statistics <- gen_data %>%
    dplyr::left_join(
      obs_data, by = c('lat', 'lon', 'year'), suffix = c('.gen', '.obs')
    ) %>%
    dplyr::filter(
      # Fabricio solo usa estos datos observados en la planilla excel, los demas los descarta!
      dplyr::between(year, first_training_year, last_training_year)  
    ) %>%
    dplyr::mutate(
      # Fabricio redondea los datos observados a un solo decimal
      value.obs = round2(value.obs, 1)  
    ) %>%
    dplyr::group_by(lat, lon) %>%
    dplyr::summarise(
      mean = mean(value.obs, na.rm = TRUE), # media, excel pt: AVERAGE
      var = var(value.obs, na.rm = TRUE), # varianza, excel pt: VAR
      corr = cor(value.gen, value.obs, use = 'na.or.complete'), # correlation, excel pt: CORREL
      .groups = 'drop'
    ) %>%
    dplyr::ungroup()
  
  # Se agregan, a los datos generados, la media, la varianza y la correlación 
  # calculados en el paso previo. Luego se calculan las anomalias (anom), las 
  # previsiones normalizadas (prev_norm), la distancia de la previsión en
  # relación a la distribución observada (dp) y las probabilidades de que la
  # precipitación sea menor, igual o mayor a la media.
  data <- gen_data %>%
    dplyr::select(lat, lon, year, value.gen = value) %>%
    dplyr::left_join(data_obs_statistics, by = c('lat', 'lon')) %>%
    dplyr::group_by(lat, lon) %>%
    dplyr::mutate(
      prev_norm = (value.gen - mean) / sqrt(var),
      anom = value.gen - mean
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      dp = sqrt(1 - corr^2)  # distancia de la previsión en relación a la distribución observada
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
      pr.media.round = round2(pr.media, 1),
      pr.media.diff = pr.media - pr.media.round,
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
    dplyr::mutate(  # si no se redondea puede seleccionarse mal el código de color
      pr.menor.media.ajustado = round2(pr.menor.media.ajustado, 2),
      pr.media.ajustado = round2(pr.media.ajustado, 2),
      pr.mayor.media.ajustado = round2(pr.mayor.media.ajustado, 2),
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

  # Remover objetos que ya no se van a utilizar
  rm(data_obs_statistics); gc()
  
  
  
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
  

  if (fp$swap_years) {
    # En caso que los años deban se renombrados, se hace lo siguiente:
  
    # Identificar el año inmediatamente posterior al periodo de entrenamiento
    min_date_in_data <- gen_data %>% 
      dplyr::filter(year > last_training_year) %>%
      dplyr::pull(year) %>% min()
    # Calcular desfasaje entre el año inmediatamente posterior al periodo 
    # de entrenamiento y el primer año de pronóstico
    desfasaje <- first_fcst_year - min_date_in_data
    
    # Definir un mapeo que permita renombrar los años
    mapeo <- purrr::map_dfr(
      .x = gen_data %>% 
        dplyr::filter(year > last_training_year) %>%
        dplyr::pull(year) %>% unique(),
      .f = function(year) {
        tibble::tibble(orig_year = year, new_year = year + desfasaje)
      })
    # Renombrar los años segun corresponda y descartar los años de entrenamiento
    data <- data %>%
      dplyr::filter(year > last_training_year) %>%
      dplyr::left_join(mapeo, by = c('year' = 'orig_year')) %>%
      dplyr::mutate(new_year = ifelse(is.na(new_year), year, new_year)) %>%
      dplyr::mutate(year = new_year) %>%
      dplyr::select(-new_year) %>%
      dplyr::arrange(year)
    
    # Remover objetos que ya no se van a utilizar
    rm(min_date_in_data, desfasaje, mapeo); gc()
  
  } else {
    # En caso que los años NO deban se renombrados, se hace lo siguiente:
    
    data <- data %>%
      dplyr::filter(year >= first_fcst_year, year <= last_fcst_year) 
    
  }
  
  
  
  #
  # COMPARAR CON LOS DATOS EN LAS PLANILLAS DE FABRICIO
  #
  
  # data_comp <- purrr::map_dfr(
  #   .x = unique(data$year),
  #   .f = function(data_year) {
  #     DADOS_ANOM <- read.table(glue::glue("TXT/ANOM_PREC_NMME_{data_year}.txt"), header = T, sep="") %>%
  #       dplyr::select(lon = Lon, lat = Lat, anom = ANOM) %>%
  #       dplyr::mutate(year = data_year)
  #     DADOS_CORR <- read.table(glue::glue("TXT/CORR_PREC_NMME.txt"), header = T, sep="") %>%
  #       dplyr::select(lon = Lon, lat = Lat, corr = CORREL) %>%
  #       dplyr::mutate(year = data_year)
  #     DADOS_PREV <- read.table(glue::glue("TXT/PREV_PREC_NMME_{data_year}.txt"), header = T, sep="") %>%
  #       dplyr::select(lon = Lon, lat = Lat, prev = PREV) %>%
  #       dplyr::mutate(year = data_year)
  #     DADOS_PROB <- read.table(glue::glue("TXT/PROB_PREC_NMME_{data_year}.txt"), header = T, sep="") %>%
  #       dplyr::select(lon = Lon, lat = Lat, codigo_de_color = PROB) %>%
  #       dplyr::mutate(year = data_year)
  # 
  #     data %>%
  #       dplyr::filter(year == data_year) %>%
  #       dplyr::select(lat, lon, year, anom, corr, prev = value.gen, codigo_de_color) %>%
  #       dplyr::mutate(anom = round2(anom, 0), corr = round2(corr, 2), prev = round2(prev, 0)) %>%
  #       dplyr::left_join(DADOS_ANOM, by = c("lon", "lat", "year"), suffix = c("", ".fab")) %>%
  #       dplyr::left_join(DADOS_CORR, by = c("lon", "lat", "year"), suffix = c("", ".fab")) %>%
  #       dplyr::left_join(DADOS_PREV, by = c("lon", "lat", "year"), suffix = c("", ".fab")) %>%
  #       dplyr::left_join(DADOS_PROB, by = c("lon", "lat", "year"), suffix = c("", ".fab"))
  # 
  #   }) %>%
  #   dplyr::select(lat, lon, year, anom, anom.fab, corr, corr.fab,
  #                     prev, prev.fab, codigo_de_color, codigo_de_color.fab) %>%
  #   dplyr::filter(corr != corr.fab | codigo_de_color != codigo_de_color.fab) %>%
  #   dplyr::filter(codigo_de_color != codigo_de_color.fab)
  
  
  
  #
  # GENERAR GRÁFICOS/MAPAS
  #
  
  for (data_year in unique(data$year)) {
    
    # Graficos simples (requieren una sola interpolación)
    # En general son gráficos similares a los realizados por Fabricio Santos,
    # de hecho, además de los gráficos generados con leaflet y ggplot, se 
    # generan gráficos identicos a los de Fabricio. Para la probabilidades se
    # utiliza el mapeo de propuesto por Fabricio, lo que permite generar una 
    # sola variable que representa la probabilidades de precipitación y 
    # temperatura menor a la media (1), igual a la media (2) y superior a la 
    # media (3). La ventaja de este enfoque es que, al tener una sola variable
    # en lugar de 3, se requiere de una sola interpolación. La desventaja es 
    # la complejidad del mapeo y la pérdida de información con respecto a la 
    # probabilidad igual a la media (solo se sabe que es mayor a 40%, nada más)
    for (data_type in c("anom", "corr", "value.gen", 
                        "codigo_de_color", "value.fcst")) {
      print(glue::glue("data_type == '{data_type}'"))
      
      ##########################################################################
      ## Procesar datos a graficar  ##################
      
      if (data_type == "value.fcst") {
        datos <- fcst_data %>% 
          dplyr::filter(year == data_year) %>% 
          dplyr::mutate(lon = ifelse(lon > 180, -360 + lon, lon)) %>%
          dplyr::select(lon, lat, var = value) %>% 
          dplyr::filter(!is.na(var))
      } else {
        datos <- data %>% 
          dplyr::filter(year == data_year) %>% 
          dplyr::select(lon, lat, var = !!data_type) %>% 
          dplyr::filter(!is.na(var))
      }   
      
      # Redonder como en las planillas excel
      if (data_type == "anom") {
        datos <- datos %>%
          dplyr::mutate(var = round2(var, 0))
      } else if (data_type == "corr") {
        datos <- datos %>%
          dplyr::mutate(var = round2(var, 2))
      } else if (data_type == "value.gen") {
        datos <- datos %>%
          dplyr::mutate(var = round2(var, 0))
      } else if (data_type == "value.fcst") {
        datos <- datos %>%
          dplyr::mutate(var = round2(var, 0))
      }
      
      # Guardar datos como sf 
      datos.sf <- sf::st_as_sf(datos, coords = c("lon", "lat")) %>%
        sf::st_set_crs(sf::st_crs(4326))
      ##########################################################################
      
      ##########################################################################
      ## Generar una grilla regular ##################
      resolucao <- ifelse(data_type == "value.fcst", 1, 0.5)
      # x.range <- as.numeric(c(-78.75, -35.75))
      # y.range <- as.numeric(c(-59.75, -9.75))
      x.range <- as.numeric(c(config$spatial_domain$wlo, config$spatial_domain$elo))  # min/max longitude of the interpolation area
      y.range <- as.numeric(c(config$spatial_domain$sla, config$spatial_domain$nla))
      grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resolucao), 
                         y = seq(from = y.range[1], to = y.range[2], by = resolucao))  # expand points to grid
      sp::coordinates(grd) <- ~x + y
      sp::gridded(grd) <- TRUE
      #
      datos.sp <- datos
      sp::coordinates(datos.sp) <- ~lon + lat  ## Convertendo data.frame para SpatialPointsData.frame
      idw <- gstat::idw(formula = var ~ 1, locations = datos.sp, newdata = grd)
      idw.output <- raster::as.data.frame(idw)  # Convertendo para um data.frame
      names(idw.output) <- c("lon", "lat", "var") 
      idw.r <- raster::rasterFromXYZ(idw.output[,1:3])
      idw.crp <- raster::crop(idw.r, crcsas_sp)
      idw.msk <- raster::mask(idw.crp, crcsas_sp)
      raster::crs(idw.msk) <- "EPSG:4326"
      idw.msk.dfr <- raster::as.data.frame(raster::rasterToPoints(idw.msk))
      idw.msk.sf <- sf::st_as_sf(idw.msk.dfr, coords = c("x", "y")) %>%
        sf::st_set_crs(sf::st_crs(4326))
      
      # COMPARACIÓN VISUAL DE PUNTOS A INTERPOLAR Y PUNTOS GENERADOS POR CPT
      # plot(sf::st_geometry(idw.msk.sf), col = "red", cex = .7)
      # plot(sf::st_geometry(datos.sf), col = "blue", cex = .5, add = T)
      # CONCLUSIÓN: 
      # 1. La interpolacion se hace para completar puntos faltantes, por
      # ejemplo, puntos muy al sur, que no se generan al usar chirps!
      # 2. Luego de tranformar los puntos originales a raster, interpolalos
      # y volver a obtener trasnformar el raster a puntos, se obtienen los 
      # mismos puntos (no pude identificar cómo se crea el raster a partir 
      # del punto original, es decir si se lo usa como centro, extremo 
      # izquierdo superior, etc.)
      
      ##########################################################################
      
      ##########################################################################
      ## Generar atributos globales de los gráficos ##################
      if (first_fcst_month > initial_month_int) {
        month_year <- glue::glue("{month.abb[first_fcst_month]} {data_year}")
      } else {
        month_year <- glue::glue("{month.abb[first_fcst_month]} {data_year+1}")
      }
      if (fp$type == "seasonal") {
        if (last_fcst_month > initial_month_int) {
          month_year <- glue::glue("{month_year} - {month.abb[last_fcst_month]} {data_year}")
        } else {
          month_year <- glue::glue("{month_year} - {month.abb[last_fcst_month]} {data_year+1}")
        }
      } 
      if (data_type == "anom") {
        main_title <- glue::glue("{variable_str} Anomaly Forecast ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {month_year} ",
                                 "\nIssued: {initial_month} {data_year}")
        fig_file_name <- paste0(
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_anom')
      } else if (data_type == "corr") {
        main_title <- glue::glue("Correlation between Forecast and Observation ",
                                 "({first_training_year}-{last_training_year})",
                                 "\n{toupper(modelo)} valid for {forecast_months_str} ",
                                 "\nIssued: {initial_month}")
        fig_file_name <- paste0(
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_corr')
      } else if (data_type == "value.gen") {  # PREV_PREC
        main_title <- glue::glue("{variable_str} ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {month_year} ",
                                 "\nIssued: {initial_month} {data_year} ",
                                 "\n(calibrated forecast)")
        fig_file_name <- paste0(
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_prev')
      } else if (data_type == "codigo_de_color") {  # PROB_PREC
        main_title <- glue::glue("{variable_str} - Probability Forecast ",
                                 "\n{toupper(modelo)} valid for {month_year} ",
                                 "\nIssued: {initial_month} {data_year}")
        fig_file_name <- paste0(
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_prob')
      } else if (data_type == "value.fcst") {
        main_title <- glue::glue("{variable_str} ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {month_year} ",
                                 "\nIssued: {initial_month} {data_year} ",
                                 "\n(uncalibrated forecast)")
        fig_file_name <- paste0(
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_fcst')
      }
      ##########################################################################
      
      ##########################################################################
      # Graficar datos (graficos de Fabricio)  #############
      generar_graficos_fabricio(data_type, idw.msk.dfr, 
                                variable,
                                paises_sp, 
                                fig_file_name, 
                                config)
      ##########################################################################
      
      ##########################################################################
      # Graficar datos (escalas discretas)  #############
      generar_graficos_discretos(data_type, idw.msk.dfr, 
                                 variable, variable_fcst, variable_str,
                                 crcsas_sf, crcsas_sp, 
                                 main_title, fig_file_name,
                                 config)
      ##########################################################################
      
      ##########################################################################
      # Graficar datos (escalas continuas)  #############
      generar_graficos_continuos(data_type, idw.msk.dfr,
                                 variable, variable_fcst, variable_str,
                                 crcsas_sf, crcsas_sp,
                                 main_title, fig_file_name,
                                 config)
      ##########################################################################
      
    }
    
    # Graficos complejos (requieren varias interpolaciones)
    # Por ahora solo están invlucradas la probabilidades de precipitación
    # y temperatura menor a la media (1), igual a la media (2) y superior
    # a la media (3). Cada una de estas probabilidades se interpolan por 
    # separado. Luego se hacen gráficos usando estas tres interpolaciones.      
    print(glue::glue("data_type == 'prob_sep'"))
    
    ##########################################################################
    ## Procesar datos a graficar  ##################
    
    datos <- data %>% 
      dplyr::filter(year == data_year) %>% 
      dplyr::select(lon, lat, pr.menor.media, pr.media, pr.mayor.media)
    
    # Guardar datos como sf 
    datos.sf <- sf::st_as_sf(datos, coords = c("lon", "lat")) %>%
      sf::st_set_crs(sf::st_crs(4326))
    ##########################################################################
    
    ##########################################################################
    ## Generar una grilla regular ##################
    
    resolucao <- 0.5
    # x.range <- as.numeric(c(-78.75, -35.75))
    # y.range <- as.numeric(c(-59.75, -9.75))
    x.range <- as.numeric(c(config$spatial_domain$wlo, config$spatial_domain$elo))  # min/max longitude of the interpolation area
    y.range <- as.numeric(c(config$spatial_domain$sla, config$spatial_domain$nla))
    grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resolucao), 
                       y = seq(from = y.range[1], to = y.range[2], by = resolucao))  # expand points to grid
    sp::coordinates(grd) <- ~x + y
    sp::gridded(grd) <- TRUE
    
    # Interpolar pr.menor.media
    datos.sp <- datos %>% 
      dplyr::select(lon, lat, var = pr.menor.media) %>% 
      dplyr::filter(!is.na(var))
    sp::coordinates(datos.sp) <- ~lon + lat  ## Convertendo data.frame para SpatialPointsData.frame
    idw <- gstat::idw(formula = var ~ 1, locations = datos.sp, newdata = grd)
    idw.output <- raster::as.data.frame(idw)  # Convertendo para um data.frame
    names(idw.output) <- c("lon", "lat", "var") 
    idw.r <- raster::rasterFromXYZ(idw.output[,1:3])
    idw.crp <- raster::crop(idw.r, crcsas_sp)
    idw.msk <- raster::mask(idw.crp, crcsas_sp)
    raster::crs(idw.msk) <- "EPSG:4326"
    #
    idw.pr.menor.media.msk <- idw.msk
    idw.pr.menor.media.dfr <- idw.msk %>% 
      raster::rasterToPoints() %>% 
      raster::as.data.frame()
    idw.pr.menor.media.sf <- idw.pr.menor.media.dfr %>% 
      sf::st_as_sf(coords = c("x", "y")) %>%
      sf::st_set_crs(sf::st_crs(4326))
    
    # COMPARACIÓN VISUAL DE PUNTOS A INTERPOLAR Y PUNTOS GENERADOS POR CPT
    # plot(
    #   sf::st_geometry(idw.pr.menor.media.sf),
    #   col = "red", cex = .7)
    # plot(
    #   sf::st_geometry(datos.sf %>% dplyr::select(lon, lat, var = pr.menor.media)), 
    #   col = "blue", cex = .5, add = T)
    
    # Interpolar pr.media
    datos.sp <- datos %>% 
      dplyr::select(lon, lat, var = pr.media) %>% 
      dplyr::filter(!is.na(var))
    sp::coordinates(datos.sp) <- ~lon + lat  ## Convertendo data.frame para SpatialPointsData.frame
    idw <- gstat::idw(formula = var ~ 1, locations = datos.sp, newdata = grd)
    idw.output <- raster::as.data.frame(idw)  # Convertendo para um data.frame
    names(idw.output) <- c("lon", "lat", "var") 
    idw.r <- raster::rasterFromXYZ(idw.output[,1:3])
    idw.crp <- raster::crop(idw.r, crcsas_sp)
    idw.msk <- raster::mask(idw.crp, crcsas_sp)
    raster::crs(idw.msk) <- "EPSG:4326"
    #
    idw.pr.media.msk <- idw.msk
    idw.pr.media.dfr <- idw.msk %>%
      raster::rasterToPoints() %>%
      raster::as.data.frame()
    idw.pr.media.sf <- idw.pr.media.dfr %>%
      sf::st_as_sf(coords = c("x", "y")) %>%
      sf::st_set_crs(sf::st_crs(4326))
    
    # COMPARACIÓN VISUAL DE PUNTOS A INTERPOLAR Y PUNTOS GENERADOS POR CPT
    # plot(
    #   sf::st_geometry(idw.pr.media.sf),
    #   col = "red", cex = .7)
    # plot(
    #   sf::st_geometry(datos.sf %>% dplyr::select(lon, lat, var = pr.media)), 
    #   col = "blue", cex = .5, add = T)
    
    # Interpolar pr.menor.media
    datos.sp <- datos %>% 
      dplyr::select(lon, lat, var = pr.mayor.media) %>% 
      dplyr::filter(!is.na(var))
    sp::coordinates(datos.sp) <- ~lon + lat  ## Convertendo data.frame para SpatialPointsData.frame
    idw <- gstat::idw(formula = var ~ 1, locations = datos.sp, newdata = grd)
    idw.output <- raster::as.data.frame(idw)  # Convertendo para um data.frame
    names(idw.output) <- c("lon", "lat", "var") 
    idw.r <- raster::rasterFromXYZ(idw.output[,1:3])
    idw.crp <- raster::crop(idw.r, crcsas_sp)
    idw.msk <- raster::mask(idw.crp, crcsas_sp)
    raster::crs(idw.msk) <- "EPSG:4326"
    #
    idw.pr.mayor.media.msk <- idw.msk
    idw.pr.mayor.media.dfr <- idw.msk %>% 
      raster::rasterToPoints() %>% 
      raster::as.data.frame()
    idw.pr.mayor.media.sf <- idw.pr.mayor.media.dfr %>% 
      sf::st_as_sf(coords = c("x", "y")) %>%
      sf::st_set_crs(sf::st_crs(4326))
    
    # COMPARACIÓN VISUAL DE PUNTOS A INTERPOLAR Y PUNTOS GENERADOS POR CPT
    # plot(
    #   sf::st_geometry(idw.pr.mayor.media.sf),
    #   col = "red", cex = .7)
    # plot(
    #   sf::st_geometry(datos.sf %>% dplyr::select(lon, lat, var = pr.mayor.media)), 
    #   col = "blue", cex = .5, add = T)
    
    idw.pr.dfr <- idw.pr.menor.media.dfr %>% 
      dplyr::rename(pr.menor.media = var) %>%
      dplyr::left_join(idw.pr.media.dfr, 
                       by = c("x", "y")) %>% 
      dplyr::rename(pr.media = var) %>%
      dplyr::left_join(idw.pr.mayor.media.dfr, 
                       by = c("x", "y")) %>%
      dplyr::rename(pr.mayor.media = var) %>%
      dplyr::mutate(pr.sum = pr.menor.media + pr.media + pr.mayor.media) %>%
      dplyr::rename(lon = x, lat = y) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(pr.max.col = dplyr::case_when(
        pr.menor.media > pr.media && pr.menor.media > pr.mayor.media ~ "pr.menor.media",
        pr.media >= pr.menor.media && pr.media >= pr.mayor.media ~ "pr.media",
        pr.mayor.media > pr.media && pr.mayor.media > pr.menor.media ~ "pr.mayor.media",
        TRUE ~ NA_character_)) %>%
      dplyr::mutate(pr = dplyr::case_when(
        pr.max.col == "pr.menor.media" ~ pr.menor.media*100 + 100,
        pr.max.col == "pr.media" ~ pr.media*100 + 200,
        pr.max.col == "pr.mayor.media" ~pr.mayor.media*100 + 300,
        TRUE ~ NA_real_)) %>%
      dplyr::ungroup()
    
    # CONCLUSIÓN: 
    # 1. La interpolacion se hace para completar puntos faltantes, por
    # ejemplo, puntos muy al sur, que no se generan al usar chirps!
    # 2. Luego de tranformar los puntos originales a raster, interpolalos
    # y volver a obtener trasnformar el raster a puntos, se obtienen los 
    # mismos puntos (no pude identificar cómo se crea el raster a partir 
    # del punto original, es decir si se lo usa como centro, extremo 
    # izquierdo superior, etc.)
    
    ##########################################################################
    
    ##########################################################################
    ## Generar atributos globales de los gráficos ##################
    if (first_fcst_month > initial_month_int) {
      month_year <- glue::glue("{month.abb[first_fcst_month]} {data_year}")
    } else {
      month_year <- glue::glue("{month.abb[first_fcst_month]} {data_year+1}")
    }
    if (fp$type == "seasonal") {
      if (last_fcst_month > initial_month_int) {
        month_year <- glue::glue("{month_year} - {month.abb[last_fcst_month]} {data_year}")
      } else {
        month_year <- glue::glue("{month_year} - {month.abb[last_fcst_month]} {data_year+1}")
      }
    } 
    main_title <- glue::glue("{variable_str} - Probability Forecast ",
                             "\n{toupper(modelo)} valid for {month_year} ",
                             "\nIssued: {initial_month} {data_year}")
    fig_file_name <- paste0(
      stringr::str_replace(fp$file, '.txt', ''),
      '_', data_year, '_prob_sep')
    ##########################################################################
    
    ##########################################################################
    # Definir paleta de colores de la NOAA  #############
    
    # Ver: https://www.weather.gov/news/211409-temperature-precipitation-maps
    noaa_escala_azules <- 
      c("#bdc9e3", "#98c1df", "#81b5e3", "#2c9dd5", "#075f9d", "#2d2a6e", "#1d2350")
    noaa_escala_azules <- 
      tail(RColorBrewer::brewer.pal(9, "PuBu"), 7)
    noaa_escala_rojos <- 
      c("#e6b06a", "#e38c4f", "#da5831", "#c52f29", "#c9303e", "#7c2f35", "#662a2e")
    noaa_escala_rojos <- 
      tail(RColorBrewer::brewer.pal(9, "YlOrBr"), 7)
    ##########################################################################
    
    ##########################################################################
    # Graficar datos (graficos de probabilidades separadas)  #############
    generar_graficos_prob_sep_discretos_op_1()
    generar_graficos_prob_sep_continuos_op_1()
    generar_graficos_prob_sep_discretos_op_2()
    generar_graficos_prob_sep_continuos_op_2()
    ##########################################################################
    
  }

}
# ------------------------------------------------------------------------------
