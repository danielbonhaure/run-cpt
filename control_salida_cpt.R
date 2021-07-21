
rm(list = ls()); gc()

library(dplyr, quietly = TRUE)

# Definir path absoluto al archivo
file_abs_path <- paste0(getwd(), '/output/nmme_precip-prcp_Mayic_6_1982-2010_2020-2021.txt')
file_abs_path <- paste0(getwd(), '/output/nmme_precip-prcp_Apric_5_1982-2010_2020-2021.txt')
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

# Definir path absoluto al archivo
file_abs_path <- paste0(getwd(), '/output/nmme_precip-prcp_Mayic_6_1982-2010_2020-2021_fabricio.txt')
file_abs_path <- paste0(getwd(), '/output/nmme_precip-prcp_Apric_5_1982-2010_2020-2021_fabricio.txt')
# Extraer latitudes (usar nombre de columna como ID)
y <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 3, nrows = 1)
yy <- y %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lat") %>% 
  dplyr::select(-V1)
# Extraer longitudes (usar nombre de columna como ID)
x <- read.table(file =file_abs_path, sep = '\t', header = FALSE, skip = 4, nrows = 1)
xx <- x %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = "lon") %>% 
  dplyr::select(-V1)
# Extraer valores (usar año y columna como ID)
c <- read.table(file = file_abs_path, sep = '\t', header = FALSE, skip = 5, dec = ",")
cc <- c %>% tidyr::pivot_longer(!V1, names_to = "columna", values_to = 'value') %>% 
  dplyr::rename(year = V1) %>%
  dplyr::mutate(value = ifelse(value == -999, NA, value)) 

# Unir los datos extraídos en único dataframe largo (es más facil hacer calculos estadísticos así, con groupby)
fab_data <- dplyr::left_join(yy, xx, by = 'columna') %>% 
  dplyr::left_join(cc, by = 'columna') %>% 
  dplyr::select(-columna)

# Remover objetos que ya no se van a utilizar
rm(file_abs_path, y, yy, x, xx, c, cc); gc()



comp_full <- fab_data %>% 
  dplyr::inner_join(gen_data, by = c("lat", "lon", "year"), suffix = c(".fab", ".gen")) %>%
  dplyr::mutate(diff = value.fab - value.gen) %>%
  dplyr::filter(diff > 1) %>% dplyr::arrange(lat, lon)


crcsas <- sf::st_read("./input/raw_data/shapefiles/CRC_SAS.shp")
crcsas <- sf::st_set_crs(crcsas, 4326)
puntos <- sf::st_as_sf(comp_full %>% dplyr::select(lon, lat, diff), coords = c('lon', 'lat'))
puntos <- sf::st_set_crs(puntos, 4326)
library(sf)
plot(sf::st_geometry(crcsas))
plot(sf::st_geometry(puntos), add=T)
                      