
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

# Leer shapes a ser utlizados en los gráficos
crcsas_sp <- sf::as_Spatial(sf::st_read(paste0(config$folders$shapefiles, "/CRC_SAS.shp")))
pais_sp <- as(sf::st_read(paste0(config$folders$shapefiles, "/10m_admin_0_countries.shp")), 'Spatial')

for (fp in config$files) {

  #
  # PARSEAR NOMBRE DEL ARCHIVO
  #
  
  # fp <- config$files[[1]]
  # fp$file <- "nmme_precip-prcp_Mayic_6_1982-2010_2020-2021.txt"

  fp_split <- stringr::str_split_fixed(stringr::str_replace(fp$file, '.txt', ''), '_', 6)
  
  modelo <- fp_split[1]
  variable <- stringr::str_split_fixed(fp_split[2], '-', 2)[2]
  variable_str <- ifelse(variable == 'prcp', 'Precipitation', 'Average Temperature')
  variable_unit <- ifelse(variable == 'prcp', 'mm', '°C')
  variable_fcst <- ifelse(variable == 'prcp', 'precip', 'tmp2m')
  initial_month <- stringr::str_replace(fp_split[3], 'ic', '')
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
      corr = cor(value.gen, value.obs, use = 'na.or.complete') # correlation, excel pt: CORREL
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

  # # Graficar datos pronosticados sin calibrar
  # crcsas_sf <- sf::st_read(
  #   paste0(config$folders$shapefiles, "/CRC_SAS.shp")) %>%
  #   sf::st_transform(crs = 4326) %>%
  #   sf::st_geometry()
  # # El problema a resolver es que datos_sf va de 0 a 360, mientras
  # # que crcsas_sf va de -180 a 180. Solución: restar 180 a lon.
  # datos_sf <- fcst_data %>% 
  #   dplyr::filter(year == data_year) %>% 
  #   dplyr::select(-year) %>%
  #   dplyr::mutate(lon = lon - 180) %>%
  #   sf::st_as_sf(coords = c('lon', 'lat'), crs = 4326)  %>% 
  #   sf::st_filter(crcsas_sf)
  
  library(lattice, quietly = TRUE)
  
  for (data_year in unique(data$year)) {
    
    # Graficar datos pronosticados calibrados
    for (data_type in c("anom", "corr", "value.gen", 
                        "codigo_de_color", "value.fcst")) {
      
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
      
      ##########################################################################
      ## Gerando uma grade regular ##################
      resolucao <- ifelse(data_type == "value.fcst", 1, 0.5)
      x.range <- as.numeric(c(config$spatial_domain$wlo, config$spatial_domain$elo))  # min/max longitude of the interpolation area
      y.range <- as.numeric(c(config$spatial_domain$sla, config$spatial_domain$nla))
      grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resolucao), 
                         y = seq(from = y.range[1], to = y.range[2], by = resolucao))  # expand points to grid
      sp::coordinates(grd) <- ~x + y
      sp::gridded(grd) <- TRUE
      sp::coordinates(datos) <- ~lon + lat  ## Convertendo data.frame para SpatialPointsData.frame
      idw <- gstat::idw(formula = var ~ 1, locations = datos, newdata = grd)
      idw.output <- raster::as.data.frame(idw)  # Convertendo para um data.frame
      names(idw.output) <- c("lon", "lat", "var") 
      idw.r <- raster::rasterFromXYZ(idw.output[,1:3])
      idw.crp <- raster::crop(idw.r, crcsas_sp)
      idw.msk <- raster::mask(idw.crp, crcsas_sp)
      raster::crs(idw.msk) <- "EPSG:4326"
      idw.msk.dfr <- raster::as.data.frame(raster::rasterToPoints(idw.msk))
      idw.msk.sf <- sf::st_as_sf(idw.msk.dfr, coords = c("x", "y"), remove = F) %>%
        sf::st_set_crs(sf::st_crs(4326)) %>% dplyr::rename(lon = x, lat = y)
      
      ##########################################################################
      ## Generar atributos de los gráficos ##################
      if (data_type == "anom") {
        if (variable == 'prcp') {
          seqq <- c(-200,-100,-50,-20,-10,-5,5,10,20,50,100,200)
          seq.l <- c(-200,-100,-50,-20,-10,-5,5,10,20,50,100,200)
        } else if (variable == 't2m') {
          seqq <- c(-4,-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2,4)
          seq.l <- c(-4,-2,-1,-0.5,-0.3,-0.1,0.1,0.3,0.5,1,2,4)
        }
        main_title <- glue::glue("{variable_str} Anomaly Forecast ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {forecast_months_str}",
                                 "\nIssued: {initial_month} {data_year}")
        paleta <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7',
                    '#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac',
                    '#053061','#1d0036')
        if (variable == 't2m')
          paleta <- rev(paleta)
        teste <- grDevices::colorRampPalette(paleta)
        par_settings <- rasterVis::rasterTheme(region=teste(14))
        col_regions  <- NULL
        fig_file_name <- paste0(
          config$folders$output, 
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_anom')
      } else if (data_type == "corr") {
        seqq <- c(-1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
        seq.l <- c(-1,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
        main_title <- glue::glue("Correlation between Forecast and Observation ",
                                 "({first_training_year}-{last_training_year})",
                                 "\n{toupper(modelo)} valid for {forecast_months_str}",
                                 "\nIssued: {initial_month}")
        paleta <- c("#ff6766","#ff9899","#fecccb","#99ffff","#00ffff",
                    "#00ccff","#99cccd","#6599ff","#6665fe","#0000fe",
                    "#010189")
        teste <- grDevices::colorRampPalette(paleta)
        par_settings <- rasterVis::rasterTheme(region=teste(11))
        col_regions  <- NULL
        fig_file_name <- paste0(
          config$folders$output, 
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_corr')
      } else if (data_type == "value.gen") {  # PREV_PREC
        if (variable == 'prcp') {
          seqq <- c(0,1,25,50,100,150,200,250,300,400,500)
          seq.l <- c(0,1,25,50,100,150,200,250,300,400,500)
        } else if (variable == 't2m') {
          seqq <- c(-10,8,10,12,14,16,18,20,22,24,26,28,30,32)
          seq.l <- c(-10,8,10,12,14,16,18,20,22,24,26,28,30,32)
        }
        main_title <- glue::glue("{variable_str} ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {forecast_months_str} ",
                                 "\nIssued: {initial_month} {data_year} ",
                                 "\n(calibrated forecast)")
        if (variable == 'prcp') {
          paleta <- c("#ff6634","#ff9934","#ffcc00","#ffffcd","#cdffcc",
                      "#9acc99","#34cc67","#33cc33","#019934","#006634",
                      "#00381d")
        } else if (variable == 't2m') {
          paleta <- c("#9a99ff","#ccccfe","#99ffff","#cdffcc","#ffffcb",
                      "#ffcb99","#ffcc00","#ff9a66","#ff9934","#cd9933",
                      "#cc6733","#ff6634","#fe0000","#c10202")
        }
        teste <- grDevices::colorRampPalette(paleta)
        par_settings <- rasterVis::rasterTheme(region=teste(10))
        col_regions  <- NULL
        fig_file_name <- paste0(
          config$folders$output, 
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_prev')
      } else if (data_type == "codigo_de_color") {  # PROB_PREC
        seqq <- c(-6,-5,-4,-3,-2,-1,1,2,3,4,5,6)
        seq.l <- c("70","60","50","45","40","35","35","40","45","50","60","70")
        main_title <- glue::glue("{variable_str} - Probability Forecast ",
                                 "\n{toupper(modelo)} valid for {forecast_months_str}",
                                 "\nIssued: {initial_month} {data_year}")
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
        fig_file_name <- paste0(
          config$folders$output, 
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_prob')
      } else if (data_type == "value.fcst") {  
        if (variable == 'prcp') {
          seqq <- c(0,1,25,50,100,150,200,250,300,400,500)
          seq.l <- c(0,1,25,50,100,150,200,250,300,400,500)
        } else if (variable == 't2m') {
          seqq <- c(-10,8,10,12,14,16,18,20,22,24,26,28,30,32)
          seq.l <- c(-10,8,10,12,14,16,18,20,22,24,26,28,30,32)
        }
        main_title <- glue::glue("{variable_str} ({variable_unit})",
                                 "\n{toupper(modelo)} valid for {forecast_months_str} ",
                                 "\nIssued: {initial_month} {data_year} ",
                                 "\n(uncalibrated forecast)")
        if (variable == 'prcp') {
          paleta <- c("#ff6634","#ff9934","#ffcc00","#ffffcd","#cdffcc",
                      "#9acc99","#34cc67","#33cc33","#019934","#006634",
                      "#00381d")
        } else if (variable == 't2m') {
          paleta <- c("#9a99ff","#ccccfe","#99ffff","#cdffcc","#ffffcb",
                      "#ffcb99","#ffcc00","#ff9a66","#ff9934","#cd9933",
                      "#cc6733","#ff6634","#fe0000","#c10202")
        }
        teste <- grDevices::colorRampPalette(paleta)
        par_settings <- rasterVis::rasterTheme(region=teste(10))
        col_regions  <- NULL
        fig_file_name <- paste0(
          config$folders$output, 
          stringr::str_replace(fp$file, '.txt', ''),
          '_', data_year, '_fcst')
      }
      
      
      ##########################################################################
      ## GRAFICOS CON LEAFLET (https://rstudio.github.io/leaflet/raster.html)
      tag.map.title <- htmltools::tags$style(htmltools::HTML("
        .leaflet-control.map-title { 
          transform: translate(-50%,20%);
          position: fixed !important;
          left: 50%;
          text-align: center;
          padding-left: 10px; 
          padding-right: 10px; 
          background: rgba(255,255,255,0.75);
          font-weight: bold;
          font-size: 14px;
        }
      "))
      title <- htmltools::tags$div(
        tag.map.title, 
        htmltools::HTML(stringr::str_replace_all(main_title, '\n', '<br>'))
      ) 
      m <- leaflet::leaflet() %>% leaflet::addTiles() %>%
        leaflet::addRasterImage(
          x = idw.msk, colors = paleta , opacity = 0.8) %>%
        leaflet::addLegend(
          colors = paleta, labels = seq.l, title = "Valores") %>%
        leaflet::addControl(
          html = title, position = "topleft", className="map-title") %>%
        leaflet::addSimpleGraticule()
      htmlwidgets::saveWidget(m, paste0(fig_file_name, ".html"), selfcontained = TRUE)
      # mapview::mapshot(m, file = paste0(fig_file_name, ".png"))
      
      
      ##########################################################################
      ## GRAFICOS CON GGPLOT GGMAP
      
      # bbox_crcsas <- c(
      #   left = config$spatial_domain$wlo, bottom = config$spatial_domain$sla+3, 
      #   right = config$spatial_domain$elo+2, top = config$spatial_domain$nla+3)
      # ggmap_crcsas <- ggmap::get_stamenmap(
      #   bbox = bbox_crcsas, zoom = 5, maptype = "toner-lite", crs = sf::st_crs(4326))
      # ggmap::ggmap(ggmap_crcsas) +
      #   ggplot2::geom_sf(data= idw.msk.sf)
      
      # ggmap_crcsas + ggmap::inset_raster(
      #   raster = raster::as.raster(idw.msk),
      #   xmin = config$spatial_domain$wlo, xmax = config$spatial_domain$elo,
      #   ymin = config$spatial_domain$sla, ymax = config$spatial_domain$nla,
      #   interpolate = FALSE)
      
      ##########################################################################
      ## GRAFICOS CON GGPLOT GGPLOT
      bbox <- list(
        left = config$spatial_domain$wlo, bottom = config$spatial_domain$sla, 
        right = config$spatial_domain$elo, top = config$spatial_domain$nla)
      
      main_title_split <- unlist(stringr::str_split(main_title, '\n'))
      sub_title <- paste0(paste(tail(main_title_split, -1), collapse = ''))
      world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
      
      calc_grupo <- function(x) {
        for (i in seq(length(seqq))) {
          if (i < length(seqq)) {
            if (x >= seqq[i] && x < seqq[i+1]) 
              return (seqq[i])
          } else {
            if (x >= seqq[i]) 
              return (seqq[i])
          }
        }
        return (NA)
      }
      
      format_group_levels <- function() {
        if (data_type == 'codigo_de_color') {
          return (c("Below", head(seqq, 5),
                    "Near", tail(head(seqq, 7), 2),
                    "Above", tail(seqq, 5)))
        }
        return (seqq)
      }
      
      format_legend_values <- function() {
        if (data_type == 'codigo_de_color') {
          return (c("white", head(paleta, 5),
                    "white", tail(head(paleta, 7), 2),
                    "white", tail(paleta, 5)))
        }
        return (paleta)
      }
      
      format_legend_labels <- function() {
        if (data_type == 'codigo_de_color')
          return (c("Below", head(seq.l, 5),
                    "Near", tail(head(seq.l, 7), 2),
                    "Above", tail(seq.l, 5)))
        if (data_type == 'corr' || data_type == 'anom')
          return (seq.l)
        resultado <- c()
        for (i in seq(length(seq.l))) {
          max_nchar <- ifelse(
            is.numeric(seq.l), max(nchar(abs(seq.l))), max(nchar(seq.l)))
          norm_val <- function(val) {
            repl_val <- ifelse(is.numeric(val), "0", " ")
            return (stringr::str_pad(val, max_nchar, side="left", pad=repl_val))
          }
          if (i < length(seq.l)) {
            e <- paste0('[', seq.l[i], ', ', seq.l[i+1], ')')
            resultado <- c(resultado, e)
          } else {
            e <- paste0('[', seq.l[i], ', ', 'Inf', ')')
            resultado <- c(resultado, e)
          }
        }
        return (resultado)
      }
      
      ggplot2::ggplot() + 
        ggplot2::geom_raster( 
          mapping = ggplot2::aes(
            x = x, y = y, 
            fill = factor(grupo, levels = format_group_levels())),
          data = idw.msk.dfr %>% dplyr::rowwise() %>%
            dplyr::mutate(grupo = calc_grupo(var)) %>%
            dplyr::ungroup(), 
          alpha = 1) +
        ggplot2::scale_discrete_manual(
          aesthetics = "fill",
          breaks = format_group_levels(),
          values = format_legend_values(), 
          labels = format_legend_labels(),
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
          fill = "black",
          alpha = 0.1) +
        ggplot2::coord_sf(
          xlim = c(bbox$left-2, bbox$right),
          ylim = c(bbox$bottom, bbox$top+3),
          expand = FALSE) +
        ggplot2::labs(fill = "") + 
        ggplot2::xlab("Longitude") + 
        ggplot2::ylab("Latitude") + 
        ggplot2::ggtitle(
          main_title_split[1], subtitle = sub_title) +
        # ggspatial::annotation_scale(
        #   location = "bl", width_hint = 0.4) +
        # ggspatial::annotation_north_arrow(
        #   location = "br", which_north = "true", 
        #   style = ggspatial::north_arrow_fancy_orienteering) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_line(
            color = gray(0.5), linetype = "dashed", size = 0.5), 
          panel.background = ggplot2::element_rect(
            fill = "aliceblue"),
          legend.key.height = ggplot2::unit(1, 'cm'))
        ggplot2::ggsave(
          filename = paste0(fig_file_name, ".png"),
          width = 20, height = 25, units = "cm", dpi = 600
          )
      
      ##########################################################################
      ## GRAFICOS DE FABRICIO
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
        latticeExtra::layer(sp::sp.lines(pais_sp, alpha=1))
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
      
    }
  }

}
# ------------------------------------------------------------------------------
