library(dplyr, quietly = T)

rm(list = ls()); gc()

config <- yaml::read_yaml(text = '
spatial_domain:
  nla: -10  # Northernmost latitude
  sla: -60  # Southernmost latitude
  wlo: -78  # Westernmost longitude
  elo: -34.1  # Easternmost longitude

folder: "input2/predictors/"

files:
 - { file: "ecmwf_precip_Octic_11-1_1991-2021_2021-2022_1.txt", type: "seasonal", swap_years: true }
 - { file: "ecmwf_precip_Octic_11_1991-2020_2021_1.txt", type: "monthly", swap_years: true }
 - { file: "ecmwf_precip_Octic_1_1992-2021_2022_1.txt", type: "monthly", swap_years: true }
 - { file: "ecmwf_precip_Octic_12_1991-2020_2021_1.txt", type: "monthly", swap_years: true }
 - { file: "ecmwf_tmp2m_Octic_11-1_1991-2021_2021-2022_1.txt", type: "seasonal", swap_years: true }
 - { file: "ecmwf_tmp2m_Octic_11_1991-2020_2021_1.txt", type: "monthly", swap_years: true }
 - { file: "ecmwf_tmp2m_Octic_1_1992-2021_2022_1.txt", type: "monthly", swap_years: true }
 - { file: "ecmwf_tmp2m_Octic_12_1991-2020_2021_1.txt", type: "monthly", swap_years: true }
')


for (fp in config$files) {
  cat("\n\n\n") 
  print(glue::glue("Processing file: {fp$file}"))
  
  # ---------------------------------------------------------------------------#
  # ---- PASO 1 PARSEAR NOMBRE DEL ARCHIVO ----
  #
  
  # fp <- config$files[[1]]
  
  fp_split <- stringr::str_split_fixed(stringr::str_replace(fp$file, '.txt', ''), '_', 7)
  
  modelo <- fp_split[1]
  variable <- ifelse(fp_split[2] == 'precip', 'prcp',
                     ifelse(fp_split[2] == 'tmp2m', 't2m', NA))
  initial_month <- stringr::str_replace(fp_split[3], 'ic', '')
  initial_month_int <- which(initial_month == month.abb)[[1]]
  forecast_months <- fp_split[4]
  first_fcst_month <- as.numeric(stringr::str_split_fixed(forecast_months, '-', 2)[1])
  last_fcst_month <- as.numeric(stringr::str_split_fixed(forecast_months, '-', 2)[2])
  training_period <- fp_split[5]
  first_training_year <- as.numeric(stringr::str_split_fixed(training_period, '-', 2)[1])
  last_training_year <- as.numeric(stringr::str_split_fixed(training_period, '-', 2)[2])
  # Para pronosticos seasonale el primer año de entrenamiento es el primer año 
  # para el primer mes y el segundo año de entrenamiento es el último año del
  # segundo mes. Los año son iguales cuando el trimestre está en el mismo año, 
  # pero cuando el primer mes está en un año (ej: octubre 1991) y el segundo 
  # mes está en el siguiente año (ej: enero 2021), entonces en realidad el 
  # último año de entrenamiento para los datos extraídos es: el último año del 
  # segundo mes menos uno (ej: 2021 - 1 = 2020). Esto es así porque en los 
  # datos extraídos se usa solo un año y es el año asociado al 1er mes!!
  if (fp$type == "seasonal")
    if (first_fcst_month > last_fcst_month)
      last_training_year <- last_training_year - 1
  first_forecast_years <- fp_split[6]
  first_fcst_year_first_month <- as.numeric(stringr::str_split_fixed(first_forecast_years, '-', 2)[1])
  first_fcst_year_last_month <- as.numeric(stringr::str_split_fixed(first_forecast_years, '-', 2)[2])
  n_forecasts <- as.numeric(fp_split[7])
  last_fcst_year_first_month <- first_fcst_year_first_month + n_forecasts - 1
  last_fcst_year_last_month <- first_fcst_year_last_month + n_forecasts - 1
  
  file_abs_path <- paste0(config$folder, fp$file)
  
  # ----------------------------------------------------------------------------

  # ---------------------------------------------------------------------------#
  # ---- PASO 2 LEER LOS DATOS EN EL ARCHIVO ----
  #
    
  # Extraer latitudes (usar nombre de columna como ID)
  x <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 5, nrows = 1)
  xx <- x %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lon") %>% 
    dplyr::select(-V1)
  # Extraer valores (usar año y columna como ID)
  cc <- purrr::map2_dfr(
    .x = c(first_training_year:last_training_year, first_fcst_year_first_month:last_fcst_year_first_month),
    .y = seq(from = 6, 
             length.out = length(c(first_training_year:last_training_year, first_fcst_year_first_month:last_fcst_year_first_month)), 
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
    dplyr::ungroup()
  
  # Si los datos descargados están en grados Kelvin, pasarlos a grados Celsius
  # Algunos archivos fueron descargados en grados Kelvin! Se agrega este control
  # por si se llega a leer alguno de ellos en algún momento!
  if (mean(fcst_data$value, na.rm = T) > 200 && variable == 't2m')
    fcst_data <- fcst_data %>% 
    dplyr::mutate(value = value - 273.15) 
  
  # Remover objetos que ya no se van a utilizar
  rm(x, xx, cc, n_days); gc()
  
  # ----------------------------------------------------------------------------
  
  # ---------------------------------------------------------------------------#
  # ---- PASO 3 GENERAR ARCHIVO CON NUEVO FORMATO ----
  # 
  
  # Definir latitudes y longitudes
  latitudes <- fcst_data %>% 
    dplyr::filter(year == first_fcst_year_first_month) %>%
    dplyr::pull(lat)
  longitudes <- fcst_data %>% 
    dplyr::filter(year == first_fcst_year_first_month) %>%
    dplyr::pull(lon)
  # Definir cuerpo
  filas <- fcst_data %>% 
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_wider(
      id_cols = c(lon, lat, year), 
      names_from = c(lon, lat), names_glue = "E({lon}_{lat})", 
      values_from = value) %>%
    dplyr::rename(Stn = year)
  # Definir fila latitud
  fila_lat <- tibble::tibble(
    val = c('Lat', latitudes), col = colnames(filas)) %>% 
    tidyr::pivot_wider(names_from = col, values_from = val)
  # Definir fila longitud
  fila_lon <- tibble::tibble(
    val = c('Lon', longitudes), col = colnames(filas)) %>% 
    tidyr::pivot_wider(names_from = col, values_from = val)
  # Unir todo
  nuevo_formato <- fila_lat %>%
    dplyr::bind_rows(fila_lon) %>%
    dplyr::bind_rows(filas)
  # Guardar archivo
  write.table(nuevo_formato, file = stringr::str_replace(file_abs_path, '.txt', '.csv'), 
              row.names = FALSE, quote = FALSE)
  
  # ----------------------------------------------------------------------------

}
