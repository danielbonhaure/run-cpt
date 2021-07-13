
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
list.of.packages <- c("yaml")
for (pack in list.of.packages) {
  if (! require(pack, character.only = TRUE)) {
    stop(paste0("Paquete no encontrado: ", pack))
  }
}

rm(pack); gc()
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
  archivo.config <- paste0(getwd(), "/config.yaml")
}

if (! file.exists(archivo.config)) {
  stop(paste0("El archivo de configuracion de ", archivo.config, " no existe\n"))
} else {
  cat(paste0("Leyendo archivo de configuracion ", archivo.config, "...\n"))
  config <- yaml::yaml.load_file(archivo.config)
}

# ii. Verificar archivo de configuración
if (is.null(config$target_season) && !is.null(config$forecast_data) ||
    !is.null(config$target_season) && is.null(config$forecast_data)) {
  stop("Error en el archivo de configuración.")
}

# iii. YAML de parametros 
if (length(args) > 1) {
  archivo.plot <- args[2]
} else {
  # No vino el archivo de configuracion por linea de comandos. Utilizo un archivo default
  archivo.plot <- paste0(getwd(), "/plot.yaml")
}
if (! file.exists(archivo.plot)) {
  stop(paste0("El archivo de parámetros ", archivo.plot, " no existe\n"))
} else {
  cat(paste0("Leyendo archivo de parámetros ", archivo.plot, "...\n"))
  config$plot <- yaml::yaml.load_file(archivo.plot)
}

rm(archivo.config, archivo.plot, args); gc()
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



# for (fp in config$plot$files) {
#   print(fp)
# }

fp <- config$plot$files[[1]]

#
# PARSEAR NOMBRE DEL ARCHIVO
#

fp_split <- stringr::str_split_fixed(fp$file, '_', 5)

modelo <- fp_split[1]
variable <- stringr::str_split_fixed(fp_split[2], '-', 2)[2]
month <- as.numeric(fp_split[4])
year <- as.numeric(stringr::str_replace(fp_split[5], '.txt', ''))


#
# EXTRAER DATOS GENERADOS POR EL SOFTWARE CPT
#

# Extraer latitudes (usar nombre de columna como ID)
y <- read.table(file = paste0(getwd(), '/output/', fp$file), sep = '\t', header = FALSE, skip = 3, nrows = 1)
yy <- y %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lat") %>% 
  dplyr::select(-V1)
# Extraer longitudes (usar nombre de columna como ID)
x <- read.table(file = paste0(getwd(), '/output/', fp$file), sep = '\t', header = FALSE, skip = 4, nrows = 1)
xx <- x %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lon") %>% 
  dplyr::select(-V1)
# Extraer valores (usar año y columna como ID)
c <- read.table(file = paste0(getwd(), '/output/', fp$file), sep = '\t', header = FALSE, skip = 5)
cc <- c %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = 'value') %>% 
  dplyr::rename(year = V1) %>%
  dplyr::mutate(value = ifelse(value == -999, NA, value)) 
   
# Unir los datos extraídos en único dataframe largo (es más facil hacer calculos estadísticos así, con groupby)
gen_data <- dplyr::left_join(yy, xx, by = 'columna') %>% 
  dplyr::left_join(cc, by = 'columna') %>% 
  dplyr::select(-columna)

# Remover objetos que ya no se van a utilizar
rm(y, yy, x, xx, c, cc); gc()


#
# EXTRAER DATOS OBSERVADOS
#

obs_file <- paste0(variable, '_', month, '.txt')
# Extraer latitudes (usar nombre de columna como ID)
y <- read.table(file = paste0(getwd(), '/input/predictands/', obs_file), sep = '\t', header = FALSE, skip = 1, nrows = 1)
yy <- y %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lat") %>% 
  dplyr::select(-V1)
# Extraer longitudes (usar nombre de columna como ID)
x <- read.table(file = paste0(getwd(), '/input/predictands/', obs_file), sep = '\t', header = FALSE, skip = 2, nrows = 1)
xx <- x %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lon") %>% 
  dplyr::select(-V1)
# Extraer valores (usar año y columna como ID)
c <- read.table(file = paste0(getwd(), '/input/predictands/', obs_file), sep = '\t', header = FALSE, skip = 5, na.strings = "-999")
cc <- c %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = 'value') %>% 
  dplyr::rename(year = V1) %>%
  dplyr::mutate(value = ifelse(value == -999, NA, value)) 

# Unir los datos extraídos en único dataframe largo (es más facil hacer calculos estadísticos así, con groupby)
obs_data <- dplyr::left_join(yy, xx, by = 'columna') %>% 
  dplyr::left_join(cc, by = 'columna') %>% 
  dplyr::select(-columna)

# Remover objetos que ya no se van a utilizar
rm(y, yy, x, xx, c, cc); gc()


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
# el problema es que la correlación lanza warnings si reemplazo todo por 0!!
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
    cor = cor(value.gen, value.obs, use = 'na.or.complete'), # correlation, excel pt: CORREL
    prev_norm = (value.gen - mean) / sqrt(var),
    anom = value.gen - mean
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    dp = 1 - cor^2
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
  dplyr::mutate()


# ------------------------------------------------------------------------------