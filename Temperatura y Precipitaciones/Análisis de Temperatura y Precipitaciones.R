
# Cargue de Librerías.
library(raster)
library(tidyverse)
library(sp)
library(sf)
library(rasterVis)
library(terra)
library(leaflet)


directorio <- "C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°1/Unidad N°2/Promedio_Temp" ## Cargar directorio del raster de temperatura
archivos_tif <- list.files(directorio, pattern = "_\\d{2}\\.tif$", full.names = TRUE) # obtener una lista de los nombres de archivos en el directorio 
mapas_temp <- lapply(archivos_tif, raster) # Lista raster 
mapas_por_pagina <- split(mapas_temp, ceiling(seq_along(mapas_temp)/12)) # división de la lista

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre") # Crear vector de meses

## Función de creación de mapas de temperatura a nivel mundial.
multiplot_with_red_scale <- function(mapas_temperatura, cols = 4) {
  par(mfrow = c(6, 2), mar = c(3, 3, 2, 1))
  for (i in seq_along(mapas_temperatura)) {
    col <- colorRampPalette(c("#F7FBFF", "#67000D"))(100) 
    plot(mapas_temperatura[[i]], main = paste("Temperatura promedio", meses[i]), col = col, cex.main = 2)
  }
}
# imprimir los mapas.
for (mapas in mapas_por_pagina) {
  multiplot_with_red_scale(mapas)
}
## Función de creación de mapas de precipitación mm a nivel mundial.

directorio_precipi <- "C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°1/Unidad N°2/Precipitación" ### Cargar directorio del raster de precipitación
archivos_tif_pre <- list.files(directorio_precipi, pattern = "\\.tif$", full.names = TRUE)
mapas_pre <- lapply(archivos_tif_pre, raster)
mapas_por_pagina_pre <- split(mapas_pre, ceiling(seq_along(mapas_pre)/12))
## Función de creación de mapas de precipitaciones a nivel mundial.
multiplot_with_blue_scale <- function(mapas_precipitaciones, cols = 4) {
  par(mfrow = c(6, 2), mar = c(3, 3, 2, 1))
  for (i in seq_along(mapas_precipitaciones)) {
    # Definir la paleta de colores azul
    col <- colorRampPalette(c("#d1ecf1", "#0096c7"))(100)
    plot(mapas_precipitaciones[[i]], main = paste("Precipitación promedio", meses[i]), col = col, cex.main = 2)
  }
}
for (mapas_precipitaciones in mapas_por_pagina_pre) {
  multiplot_with_blue_scale(mapas_precipitaciones)
} 


## En este apartado realizamos la carga de un shapefile el cual contiene las delimitaciones de los paises a nivel mundial
## Se gráfica el shapefile, se pone la capa del raster de temperatura óptima 
ruta_shapefile <- "C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°1/Unidad N°2/Paises_Mundo/Paises_Mundo.shp" # Ruta al archivo shapefile
paises_sf <- st_read(dsn = ruta_shapefile) # Cargar el shapefile que contiene los mapas del mundo
archivos_tif_terra <- list.files(directorio, pattern = "_\\d{2}\\.tif$", full.names = TRUE)# Obtener la lista de archivos TIFF en el directorio
# Crear una lista para almacenar las máscaras de cada mes
mascaras_meses <- list()
# Iterar sobre los archivos TIFF de cada mes
for (archivo_tif in archivos_tif_terra) {
  # Cargar el archivo TIFF correspondiente al mes
  r <- rast(archivo_tif)
  promedio_temperatura <- mean(r[r >= 22.5 & r <= 28], na.rm = TRUE)   # Calcular el promedio de temperatura dentro del rango de interés (temperatura óptima)
  mascara <- r >= promedio_temperatura - 2.5 & r <= promedio_temperatura + 2.5   # Crear una máscara que identifique los píxeles con temperatura dentro de +/- 2.5°C del promedio
  mascaras_meses[[length(mascaras_meses) + 1]] <- mascara   # Agregar la máscara a la lista de máscaras de meses
}
mascara_final <- Reduce("+", mascaras_meses) ## Contiene el mapa final


##### Creación de paises que cumplen con la temperatura óptima
colores_pastel <- c("#ffffff", "#d0200f") # Definir los colores pastel para los países dentro y fuera del rango
# Graficar el mapa con colores pastel
plot(mascara_final, col = colores_pastel)
title(main = "MAPA DE TEMPERATURA PROMEDIO ÓPTIMA")
title(xlab = "Longitud")
title(ylab = "Latitud")
plot(st_geometry(paises_sf), add = TRUE, border = "black")# Agregar la capa de los países sobre el mapa rasterizado
# Especificar las coordenadas para la leyenda
x_legend <- 100 # Esquina inferior derecha
y_legend <- -60 # Esquina inferior derecha
# Agregar la leyenda al mapa
legend(x = x_legend, y = y_legend, legend = c("Temperatura fuera del rango", "Temperatura dentro del rango"),
       fill = colores_pastel, title = "Clasificación", cex = 0.8)
# Definir los nombres de los países seleccionados y sus coordenadas en grados decimales
nombres_paises <- c("Gabón", "Guyana", "República Centroafricana")
coordenadas_paises <- c(-0.8037, 11.6094, 4.8604, -58.9302, 6.6111, 20.9394)   
distancia <- 2 # Definir la distancia para colocar el texto
text(coordenadas_paises[c(2, 4, 6)], coordenadas_paises[c(1, 3, 5)] + distancia, nombres_paises, col = "black", cex = 1.5, font = 2) # Agregar los nombres de los países al mapa como etiquetas
points(coordenadas_paises[c(2, 4, 6)], coordenadas_paises[c(1, 3, 5)], pch = 20, col = "black", cex = 1.5) # Agregar un punto para señalar la ubicación de cada país


##### Creación de paises que cumplen con las precipitaciones óptimas.
directorio_precipi <- "C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°1/Unidad N°2/Precipitación"
archivos_tif <- list.files(directorio_precipi, pattern = "_\\d{2}\\.tif$", full.names = TRUE)
rasters <- lapply(archivos_tif, raster)
promedio_mensual <- mean(stack(rasters), na.rm = TRUE)
# Cargar el raster del promedio mensual
promedio_mensual_raster <- promedio_mensual
limits <- c(0, 124, NA, 125, 290, 1, 291, Inf, NA) # Definir los límites para la reclassificación
promedio_mensual_reclass <- raster::reclassify(promedio_mensual_raster, limits) # Reclassificar el raster
mask <- promedio_mensual_reclass == 1 # Crear una máscara basada en los valores reclassificados
promedio_mensual_filtered <- raster::mask(promedio_mensual_raster, mask)# Aplicar la máscara al raster original
colores_pastel <- c("#80b9f4", "#2272d2") # Definir los colores pastel para los países dentro y fuera del rango
# Graficar el mapa de precipitación filtrado con colores pastel
plot(promedio_mensual_filtered, col = colores_pastel)
title(main = "MAPA DE PRECIPITACIÓN ÓPTIMA")
title(xlab = "Longitud")
title(ylab = "Latitud")
# Especificar las coordenadas para la leyenda
x_legend <- 100 # Esquina inferior derecha
y_legend <- -60 # Esquina inferior derecha
legend(x = x_legend, y = y_legend, legend = c("Precipitación fuera del rango", "Precipitación dentro del rango"),
       fill = colores_pastel, title = "Clasificación", cex = 0.8) # Agregar la leyenda al mapa
plot(st_geometry(paises_sf), add = TRUE, border = "black") # Agregar la capa de los países sobre el mapa de precipitación
# Definir las coordenadas de los países y sus etiquetas
coordenadas_paises <- c(4.57086,-74.297333, -0.789275,	113.921327, -0.228021, 15.8277)
nombres_paises <- c("Colombia", "Indonesia", "República Democrática del Congo")
# Definir la distancia vertical y horizontal para colocar el texto
distancia_vertical <- c(0, -0.5, -0.8)
distancia_horizontal <- c(0, 0.5, 0.5)
# Agregar los nombres de los países al mapa como etiquetas con ajuste vertical y horizontal
for (i in 1:length(nombres_paises)) {
  text(coordenadas_paises[i * 2] + distancia_horizontal[i], coordenadas_paises[i * 2 - 1] + distancia_vertical[i], nombres_paises[i], 
       col = "black", cex = 1.5, font = 2, pos = 1)
}
# Definir la distancia para colocar el texto
distancia <- 4
# Agregar un punto para señalar la ubicación de cada país
points(coordenadas_paises[2], coordenadas_paises[1], pch = 20, col = "#bd2f28", cex = 1.1)
points(coordenadas_paises[4], coordenadas_paises[3], pch = 20, col = "#bd2f28", cex = 1.1)
points(coordenadas_paises[6], coordenadas_paises[5], pch = 20, col = "#bd2f28", cex = 1.1)


## Temperatura Promedio y Precipitación anual paises ecogidos.

adm_gabon <- raster::getData(name = 'GADM', country = 'GAB', level = 1)# Obtener los límites administrativos para Gabón
adm_guyana <- raster::getData(name = 'GADM', country = 'GUY', level = 1) # Obtener los límites administrativos para Guyana
adm_centroafricana <- raster::getData(name = 'GADM', country = 'CAF', level = 1) # Obtener los límites administrativos para República Centroafricana
adm_colombia <- raster::getData(name = 'GADM', country = 'COL', level = 1) # Obtener los límites administrativos para Colombia
adm_indonesia <- raster::getData(name = 'GADM', country = 'IDN', level = 1) # Obtener los límites administrativos para Indonesia
adm_congo <- raster::getData(name = 'GADM', country = 'COD', level = 1) # Obtener los límites administrativos para República Democrática del Congo

# Función para obtener datos climáticos (temperatura media mensual)
get_climate_data <- function(country_adm, country_code) {
  crd <- as.data.frame(coordinates(country_adm))   # Obtener las coordenadas del país
  climate_data <- raster::getData(name = 'worldclim', var = 'tmean', res = 2.5, lon = crd[1, 1], lat = crd[1, 2])   # Obtener los datos climáticos para el país
  climate_data <- raster::crop(climate_data, country_adm) %>% raster::mask(., country_adm)   # Recortar y enmascarar los datos climáticos para el país
  return(climate_data)
}
tmean_gabon <- get_climate_data(adm_gabon, 'GAB') # Obtener datos climáticos para Gabón
tmean_guyana <- get_climate_data(adm_guyana, 'GUY') # Obtener datos climáticos para Guyana
tmean_centroafricana <- get_climate_data(adm_centroafricana, 'CAF') # Obtener datos climáticos para República Centroafricana
tmean_colombia <- get_climate_data(adm_colombia, 'COL') # Obtener datos climáticos para Colombia
tmean_Indonesia<- get_climate_data(adm_indonesia, 'IDN') # Obtener datos climáticos para India
tmean_Congo<- get_climate_data(adm_congo, 'COD') # Obtener datos climáticos para Congo

# Calcular temperatura media para cada país
mean_temp_gabon <- mean(tmean_gabon, na.rm = TRUE)
mean_temp_guyana <- mean(tmean_guyana, na.rm = TRUE)
mean_temp_centroafricana <- mean(tmean_centroafricana, na.rm = TRUE)
mean_temp_colombia <- mean(tmean_colombia, na.rm = TRUE)
mean_temp_Indonesia <- mean(tmean_Indonesia, na.rm = TRUE)
mean_temp_congo <- mean(tmean_Congo, na.rm = TRUE)

# Función para obtener datos climáticos (precipitación mensual)
get_climate_data_precipita <- function(country_adm, country_code) {
  # Obtener las coordenadas del país
  crd <- as.data.frame(coordinates(country_adm))
  # Obtener los datos climáticos para el país
  climate_data <- raster::getData(name = 'worldclim', var = 'prec', res = 2.5, lon = crd[1, 1], lat = crd[1, 2])
  # Recortar y enmascarar los datos climáticos para el país
  climate_data <- raster::crop(climate_data, country_adm) %>% raster::mask(., country_adm)
  return(climate_data)
}

prec_colombia <- get_climate_data_precipita(adm_colombia, 'COL') # Obtener datos climáticos para Colombia
prec_indonesia <- get_climate_data_precipita(adm_indonesia, 'IDN') # Obtener datos climáticos para Indonesia
prec_congo <- get_climate_data_precipita(adm_congo, 'COD') # Obtener datos climáticos para República Democrática del Congo 
prec_Gabon <- get_climate_data_precipita(adm_gabon, 'GAB') # Obtener datos climáticos para Gabón
prec_guyana <- get_climate_data_precipita(adm_guyana, 'GUY') # Obtener datos climáticos para guyana
prec_Centroafricana <- get_climate_data_precipita(adm_centroafricana, 'CAF') # Obtener datos climáticos para República 

# Calcular precipitación media para cada país
mean_prec_colombia <- mean(prec_colombia, na.rm = TRUE)
mean_prec_indonesia <- mean(prec_indonesia, na.rm = TRUE)
mean_prec_congo <- mean(prec_congo, na.rm = TRUE)
mean_prec_gabon <- mean(prec_Gabon, na.rm = TRUE)
mean_prec_guyana <- mean(prec_guyana, na.rm = TRUE)
mean_prec_centroafricana <- mean(prec_Centroafricana, na.rm = TRUE)

blue_red <- colorRampPalette(c("#ffa372","#ff6c3e", "#ff0000","#bd0003","#7f0000" )) ## Paleta de colores para el mapa de temperatura
blue_palette <- colorRampPalette(c("#d1e5f0","#92c5de", "#4393c3","#2166ac","#053061" )) ##  Paleta de colores para el mapa de precipitaciones.
## Precipitación y Temperatura Colombia.
par(mfrow = c(1, 2))
plot(mean_prec_colombia, main = "Precipitación Media Anual - Colombia", col = blue_palette(20),cex.main = 1.5)
plot(mean_temp_colombia, main = "Temperatura Media Anual - Colombia", col = blue_red(20),cex.main = 1.5)
## Precipitación y Temperatura Indonesia.
par(mfrow = c(1, 2))
plot(mean_prec_indonesia, main = "Precipitación Media anual - Indonesia", col = blue_palette(20),cex.main = 1.5)
plot(mean_temp_Indonesia, main = "Temperatura Media - Indonesia", col = blue_red(20),cex.main = 1.5)
### República Democrática del Congo
par(mfrow = c(1, 2))
plot(mean_prec_congo, main = "Precipitación anual - República Democrática del Congo", col = blue_palette(20),cex.main = 1.5)
plot(mean_temp_congo, main = "Temperatura Media - República Democrática del Congo", col = blue_red(20),cex.main = 1.5)
### Gabón
par(mfrow = c(1, 2))
plot(mean_prec_gabon, main = "Precipitación Media anual - Gabón", col = blue_palette(20),cex.main = 1.5)
plot(mean_temp_gabon, main = "Temperatura Media - Gabón", col = blue_red(20),cex.main = 1.5)
### Guyana
par(mfrow = c(1, 2))
plot(mean_prec_guyana, main = "Precipitación Media anual - Guyana", col = blue_palette(20),cex.main = 1.5)
plot(mean_temp_guyana, main = "Temperatura Media - Guyana", col = blue_red(20),cex.main = 1.5)
par(mfrow = c(1, 2))
### República Centroafricana.
plot(mean_prec_centroafricana, main = "Precipitación anual - República Centroafricana", col = blue_palette(20),cex.main = 1.5)
plot(mean_temp_centroafricana, main = "Temperatura Media - República Centroafricana", col = blue_red(20),cex.main = 1.5)


##### Selección de los 3 Municipios.

directorio_terra <- "C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°1/Unidad N°2/Promedio_Temp"
directorio_precipi <- "C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°1/Unidad N°2/Precipitación"
archivos_tif_terra <- list.files(directorio_terra, pattern = "_\\d{2}\\.tif$", full.names = TRUE)
archivos_tif <- list.files(directorio_precipi, pattern = "_\\d{2}\\.tif$", full.names = TRUE)
temperaturas<-stack(archivos_tif_terra)
names(temperaturas)<-month.name
preci<-stack(archivos_tif_terra)


ubicacion <- data.frame(x = c(-76.22, -76.27, -76.24), y = c(3.99, 4.09, 3.32)) ## Ubicaciones de San Pedro, Tuluá, Florida
valores_temperatura <- as.data.frame(t(as.data.frame(extract(temperaturas, ubicacion, method = 'bilinear'))))
colnames(valores_temperatura) <- c('San Pedro', 'Tuluá', 'Florida')
valores_temperatura$Month <- rownames(valores_temperatura) # Convertir los nombres de las columnas en una variable del data frame
valores_temperatura_long <- pivot_longer(valores_temperatura, 
                                         cols = c('San Pedro', 'Tuluá', 'Florida'), 
                                         names_to = "Location", 
                                         values_to = "Temperature")
valores_temperatura_long$Month <- factor(valores_temperatura_long$Month, levels = month.name)
theme_set(theme_bw()) # Definir el tema del gráfico
# Graficar los datos de temperatura por mes y ubicación como puntos y líneas
ggplot(valores_temperatura_long, aes(x = Month, y = Temperature, color = Location)) +
  geom_line(aes(group = Location)) +  # Agregamos geom_line() para trazar líneas
  geom_point(size = 2, shape = 21, fill = "white") +  # Agregamos geom_point() para mostrar puntos
  labs(title = "Variación de Temperatura por Mes",
       x = "Mes",
       y = "Temperatura °C",
       color = "Ubicación") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Cargar datos de precipitaciones
directorio_precipi <- "C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°1/Unidad N°2/Precipitación"
archivos_tif <- list.files(directorio_precipi, pattern = "_\\d{2}\\.tif$", full.names = TRUE)
preci <- stack(archivos_tif)
names(preci) <- month.name
# Extraer datos de precipitaciones en ubicaciones específicas
ubicacion_preci <- data.frame(x = c(-76.22, -76.27, -76.24), y = c(3.99, 4.09, 3.32))
valores_precipitaciones <- as.data.frame(t(as.data.frame(extract(preci, ubicacion_preci, method = 'bilinear'))))
colnames(valores_precipitaciones) <- c('San Pedro', 'Tuluá', 'Florida')
valores_precipitaciones$Month <- rownames(valores_precipitaciones) # Convertir los nombres de las columnas en una variable del data frame
# Corregir el nombre de la variable
valores_precipitaciones_long <- pivot_longer(valores_precipitaciones, 
                                             cols = c('San Pedro', 'Tuluá', 'Florida'), 
                                             names_to = "Location", 
                                             values_to = "Precipitation")

valores_precipitaciones_long$Month <- factor(valores_precipitaciones_long$Month, levels = month.name)
# Definir el tema del gráfico
theme_set(theme_bw())
# Graficar los datos de precipitaciones por mes y ubicación como puntos y líneas
ggplot(valores_precipitaciones_long, aes(x = Month, y = Precipitation, color = Location)) +
  geom_line(aes(group = Location)) +  # Agregamos geom_line() para trazar líneas
  geom_point(size = 2, shape = 21, fill = "white") +  # Agregamos geom_point() para mostrar puntos
  labs(title = "Variación de Precipitación por Mes",
       x = "Mes",
       y = "Precipitación (mm)",
       color = "Ubicación") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Obtener los límites administrativos para los municipios del Valle del Cauca
adm_colombia_municipios <- raster::getData(name = 'GADM', country = 'COL', level = 2)
Valle_del_cauca <- adm_colombia_municipios[adm_colombia_municipios$NAME_1%in% 'Valle del Cauca',]

Florida <- Valle_del_cauca[Valle_del_cauca$NAME_2%in%'Florida',]
Tuluá <- Valle_del_cauca[Valle_del_cauca$NAME_2%in%'Tuluá',]
San_Pedro <- Valle_del_cauca[Valle_del_cauca$NAME_2%in%'San Pedro',]

crd_Florida <- as.data.frame(coordinates(Florida))
crd_San_pedro <- as.data.frame(coordinates(San_Pedro))
crd_Tulua <- as.data.frame(coordinates(Tuluá))

## Temperatura
tmean_Florida <- raster::getData(name = 'worldclim',
                                 var = 'tmean',
                                 res = 0.5,
                                 lon = crd_Florida[1,1],
                                 lat = crd_Florida[1,2])

tmean_San_Pedro <- raster::getData(name = 'worldclim',
                                   var = 'tmean',
                                   res = 0.5,
                                   lon = crd_San_pedro[1,1],
                                   lat = crd_San_pedro[1,2])

tmean_Tuluá <- raster::getData(name = 'worldclim',
                               var = 'tmean',
                               res = 0.5,
                               lon = crd_Tulua[1,1],
                               lat = crd_Tulua[1,2])

tmean_VC_Tutlua <- raster::crop(tmean_Tuluá, Tuluá) %>%
  raster::mask(.,Tuluá)

tmean_VC_SP <- raster::crop(tmean_San_Pedro, San_Pedro) %>%
  raster::mask(.,San_Pedro)

tmean_VC_Florida <- raster::crop(tmean_Florida, Florida) %>%
  raster::mask(.,Florida)

blue_red <- colorRampPalette(c("#ffa372","#ff6c3e", "#ff0000","#bd0003","#7f0000" ))
blue_palette <- colorRampPalette(c("#d1e5f0","#92c5de", "#4393c3","#2166ac","#053061" ))



tmean_tutlua_promedio <- mean(tmean_VC_Tutlua)
tmean_florida_promedio <- mean(tmean_VC_Florida)
tmean_SP_promedio <- mean(tmean_VC_SP)

### Precipitación:

Pre_Florida <- raster::getData(name = 'worldclim',
                               var = 'prec',
                               res = 0.5,
                               lon = crd_Florida[1,1],
                               lat = crd_Florida[1,2])

Pre_San_Pedro <- raster::getData(name = 'worldclim',
                                 var = 'prec',
                                 res = 0.5,
                                 lon = crd_San_pedro[1,1],
                                 lat = crd_San_pedro[1,2])

Pre_Tuluá <- raster::getData(name = 'worldclim',
                             var = 'prec',
                             res = 0.5,
                             lon = crd_Tulua[1,1],
                             lat = crd_Tulua[1,2])



pre_VC_Tutlua <- raster::crop(Pre_Tuluá, Tuluá) %>%
  raster::mask(.,Tuluá)

pre_VC_SP <- raster::crop(Pre_San_Pedro, San_Pedro) %>%
  raster::mask(.,San_Pedro)

pre_VC_Florida <- raster::crop(Pre_Florida, Florida) %>%
  raster::mask(.,Florida)


Pre_tutlua_promedio <- mean(pre_VC_Tutlua)
Pre_SP_promedio <- mean(pre_VC_SP)
Pre_florida_promedio <- mean(pre_VC_Florida)
## Mapa precipitaciones y tempratura Tuluá
par(mfrow = c(1, 2))
plot(Pre_tutlua_promedio, main = "Precipitación Media Anual - Tuluá", col = blue_palette(20))
plot(tmean_tutlua_promedio, main = "Temperatura Media Anual - Tuluá", col = blue_red(20))
## Mapa precipitaciones y tempratura San Pedro.
par(mfrow = c(1, 2))
plot(Pre_SP_promedio, main = "Precipitación Media Anual - San Pedro", col = blue_palette(20))
plot(tmean_SP_promedio, main = "Temperatura Media Anual - San Pedro", col = blue_red(20))
## Mapa precipitaciones y tempratura Florida.
par(mfrow = c(1, 2))
plot(Pre_florida_promedio, main = "Precipitación Media Anual - Florida", col = blue_palette(20))
plot(tmean_florida_promedio, main = "Temperatura Media Anual - Florida", col = blue_red(20))

### Distancias Euclidianas.

##########################Distancias Euclidianas San Pedro.
San_pedro <- data.frame(longitude  = c(-76.22), ## Coordenadas San Pedro
                        latitude = c(3.99))
#Se extraen los datos de temperatura y precipitación para San Pedro del conjunto de datos principal 
temp_san_pedro<-as.data.frame( extract(temperaturas,San_pedro))
prec_san_pedro<- as.data.frame( extract(preci,San_pedro))
## Distancia Euclidiana
San_Pedro_comp_temp=sqrt(sum((temperaturas-as.numeric(temp_san_pedro))^2))
San_Pedro_comp_prec=sqrt(sum((preci-as.numeric(prec_san_pedro))^2))
Sa_Pedro_comp_temp_filtro<-San_Pedro_comp_temp
Sa_Pedro_comp_temp_filtro[(Sa_Pedro_comp_temp_filtro >28)] <- NA
Sa_Pedro_comp_prec_filtro<-San_Pedro_comp_prec
Sa_Pedro_comp_prec_filtro[(Sa_Pedro_comp_prec_filtro >290)] <- NA
#Se crea un objeto booleano interse que indica qué puntos cumplen con los criterios de temperatura y precipitación similares a San Pedro.
interse<-Sa_Pedro_comp_temp_filtro & Sa_Pedro_comp_prec_filtro
# Convertir el objeto interse a un objeto SpatialPolygonsDataFrame
interse_sp <- rasterToPolygons(interse, dissolve = TRUE)
# Crear un mapa interactivo con Leaflet
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(data = interse_sp, 
              fillColor = "blue", 
              fillOpacity = 0.5, 
              stroke = FALSE,
              popup = "País que cumple con temperatura y precipitación similares a San Pedro") %>%
  addControl(html = "<div id='map_title'>Paises que que cumplen con la temperatura y precipitaciones similares a San Pedro</div>", position = "topright")

########################## Distancias Euclidianas Florida.
Florida <- data.frame(longitude  = c(-76.24),
                      latitude = c(3.32))
#Se extraen los datos de temperatura y precipitación para San Pedro del conjunto de datos principal 
temp_Florida<-as.data.frame( extract(temperaturas,Florida))
prec_Florida<- as.data.frame( extract(preci,Florida))
## Distancia Euclidiana
Florida_comp_temp=sqrt(sum((temperaturas-as.numeric(temp_Florida))^2))
Florida_comp_prec=sqrt(sum((preci-as.numeric(prec_Florida))^2))
Florida_comp_temp_filtro<-Florida_comp_temp
Florida_comp_temp_filtro[(Florida_comp_temp_filtro >28)] <- NA
Florida_comp_prec_filtro<-Florida_comp_prec
Florida_comp_prec_filtro[(Florida_comp_temp_filtro >290)] <- NA
interse_Florida<-Florida_comp_temp_filtro & Florida_comp_prec_filtro
# Convertir el objeto interse a un objeto SpatialPolygonsDataFrame
interse_F <- rasterToPolygons(interse_Florida, dissolve = TRUE)
# Crear un mapa interactivo con Leaflet
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(data = interse_F, 
              fillColor = "pink", 
              fillOpacity = 0.5, 
              stroke = FALSE,
              popup = "País que cumple con temperatura y precipitación similares a Tuluá") %>%
  addControl(html = "<div id='map_title'>Paises que que cumplen con la temperatura y precipitaciones similares a Florida</div>", position = "topright")
## Distancias Euclidianas Tuluá.
Tulua <- data.frame(longitude  = c(-76.27),
                    latitude = c(4.09))
#Se extraen los datos de temperatura y precipitación para San Pedro del conjunto de datos principal 
temp_tulua<-as.data.frame( extract(temperaturas,Tulua))
prec_tulua<- as.data.frame( extract(preci,Tulua))
## Distancia Euclidiana
Tulua_comp_temp=sqrt(sum((temperaturas-as.numeric(temp_tulua))^2))
Tulua_comp_prec=sqrt(sum((preci-as.numeric(prec_tulua))^2))
Tulua_comp_temp_filtro<-Tulua_comp_temp
Tulua_comp_temp_filtro[(Tulua_comp_temp_filtro >28)] <- NA
Tulua_comp_prec_filtro<-Tulua_comp_prec
Tulua_comp_prec_filtro[(Tulua_comp_prec_filtro >290)] <- NA
interse_tulua<-Tulua_comp_temp_filtro & Tulua_comp_prec_filtro
# Convertir el objeto interse a un objeto SpatialPolygonsDataFrame
interse_sp_t <- rasterToPolygons(interse_tulua, dissolve = TRUE)
# Crear un mapa interactivo con Leaflet
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(data = interse_sp_t, 
              fillColor = "orange", 
              fillOpacity = 0.5, 
              stroke = FALSE,
              popup = "País que cumple con temperatura y precipitación similares a Tuluá") %>%
  addControl(html = "<div id='map_title'>Paises que que cumplen con la temperatura y precipitaciones similares a Tuluá</div>", position = "topright")
