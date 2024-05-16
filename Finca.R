library(readxl)
library(tidyverse)
library(MASS)
library(geoR)
library(leaflet)
library(rasterVis)
library(raster)
library(knitr)
library(DT)
library(kableExtra)
library(leaflet.providers)

### Cargar base de datos y crear tabla
Datos_Completos_Aguacate <- read_excel("C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°2/Unidad N°1/Datos_Completos_Aguacate.xlsx")

DT::datatable(Datos_Completos_Aguacate,
              extensions = 'FixedColumns',
              rownames= FALSE,
              options = list(
                pageLength = 10,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                autoWidth = TRUE,
                columnDefs = 
                  list(list(width = '300px', targets = c(0,7))),
                scrollX = TRUE,
                escape = T)
)

### Realizar subset de sólo la fecha de 2020-10-1

# Eliminar la hora y los segundos del formato "21/08/2019 9:36:36 a. m."
Datos_Completos_Aguacate$FORMATTED_DATE_TIME <- substr(Datos_Completos_Aguacate$FORMATTED_DATE_TIME, 1, 10)
# Convertir a fecha
Datos_Completos_Aguacate$FORMATTED_DATE_TIME <- as.Date(Datos_Completos_Aguacate$FORMATTED_DATE_TIME, format = "%d/%m/%Y")

Datos_Filtrados <- filter(Datos_Completos_Aguacate, FORMATTED_DATE_TIME == as.Date("2020-10-1"))
# Mostrar tabla
DT::datatable(Datos_Filtrados,
              extensions = 'FixedColumns',
              rownames= FALSE,
              options = list(
                pageLength = 10,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                autoWidth = TRUE,
                columnDefs = 
                  list(list(width = '300px', targets = c(0,7))),
                scrollX = TRUE,
                escape = T)
)
### Mapas de la ubicación de los árboles en la finca.

# Distribución de los puntos.
ggplot(data = Datos_Filtrados, aes(x = Longitude, y = Latitude)) +
  geom_point() +
  labs(x = "Longitud", y = "Latitud", title = "Distribución de puntos de coordenadas")
# Distribución de los puntos de forma interactiva.
leaflet() %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery) %>%
  addCircleMarkers(
    lng = Datos_Filtrados$Longitude,
    lat = Datos_Filtrados$Latitude,
    radius = 0.2,
    color = "red"
  )

# Variable regionalizada.
df_aguacate_geo <- as.geodata(Datos_Filtrados, coords.col = 3:2, data.col = 9) # Variable regionalizada Temperatura
plot(df_aguacate_geo)
# Distancias entre las coordenadas
summary(dist(df_aguacate_geo$coords))
# Variograma.

variograma <- variog(geodata = df_aguacate_geo, uvec = seq(0,0.0009,0.00005), option = "bin")
datos_env <- variog.mc.env(df_aguacate_geo, obj.variog =variograma)
plot(variograma, pch=16, envelop = datos_env)

# Ajuste Modelo.
ini.vals <- expand.grid( seq(2,4,l=10), seq(0.0001,0.00025, l = 10))

modelo_mco_exp <-  variofit(variograma, ini.vals, cov.model = "exponential")
modelo_mco_gau <- variofit(variograma, ini.vals, cov.model = "gaussian") 
modelo_mco_shp <- variofit(variograma, ini.vals, cov.model = "spherical")

plot(variograma, pch=16, envelop = datos_env)
lines(modelo_mco_exp, col = "blue")
lines(modelo_mco_gau, col = "red")
lines(modelo_mco_shp, col = "green")
legend(0.0007, 7, legend=c("Gausiano", "Exponencial", "Esferico"),
       col=c("red", "blue","green"), lty=1, cex=0.8)

# Imprimir resultados MSE por modelo
print(paste("MSE modelo Exponencial =", as.character(modelo_mco_exp$value)))
print(paste("MSE modelo Gausiano =", as.character(modelo_mco_gau$value)))
print(paste("MSE modelo Esferico =", as.character(modelo_mco_shp$value)))

# Interpolación.

# Calcula los rangos mínimos y máximos de longitud y latitud en los datos filtrados
rangos <- c(min(Datos_Filtrados[,3]),
            max(Datos_Filtrados[,3]),
            min(Datos_Filtrados[,2]),
            max(Datos_Filtrados[,2]))

## Predicción Espacial Kriging.

# Genera una cuadrícula espacial con 100 puntos a lo largo de la longitud y latitud
geodatos_grid <- expand.grid(lon = seq(rangos[1], rangos[2], length.out = 100),
                             lat = seq(rangos[3], rangos[4], length.out = 100))

# Visualiza la cuadrícula espacial
plot(geodatos_grid)

# Traza los puntos de datos geográficos sobre la cuadrícula
points(Datos_Filtrados[,3:2], col = "red")

# Realiza el Kriging con los datos de origen y la cuadrícula generada
geodatos_ko <- krige.conv(df_aguacate_geo, loc = geodatos_grid,
                          krige = krige.control(nugget = 0, trend.d = "cte",
                                                trend.l = "cte", cov.pars = c(sigmasq = 3.0186,
                                                                              phi = 0.0001)))

# Visualiza la predicción Kriging
image(geodatos_ko, main = "kriging Predict", xlab = "East", ylab = "North")
contour(geodatos_ko, main = "kriging Predict", drawlabels = TRUE)

# Visualiza la desviación estándar de la predicción Kriging
image(geodatos_ko, main = "kriging StDv Predicted", val = sqrt(geodatos_ko$krige.var), xlab = "East", ylab = "North")
contour(geodatos_ko, main = "kriging StDv Predict", val = sqrt(geodatos_ko$krige.var), drawlabels = TRUE)

# Crear un raster de predicción y visualizarlo
pred <- cbind(geodatos_grid, geodatos_ko$predict)
temp_predict <- rasterFromXYZ(pred)
plot(temp_predict)
levelplot(temp_predict, par.settings = BuRdTheme)

# Crear un raster de error y visualizarlo
temp_error <- rasterFromXYZ(cbind(geodatos_grid, sqrt(geodatos_ko$krige.var)))
levelplot(temp_error, par.settings = BuRdTheme)

# Cargar el archivo de forma de la finca y visualizarla
finca <- shapefile("~/silueta_mapa.shp")
plot(temp_predict)
plot(finca, add = TRUE)

# Crear un raster con la predicción solo dentro de los límites de la finca y visualizarlo
temp_predict_finca <- mask(temp_predict, finca)
plot(temp_predict_finca)
plot(finca, add = TRUE)

# Visualizar la predicción en forma de mapa interactivo utilizando Leaflet
leaflet() %>% 
  addTiles() %>% 
  addRasterImage(temp_predict_finca, opacity = 0.8)
# Crear un mapa interactivo con capa base de Esri.WorldImagery
mapa <- leaflet() %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery)
mapa <- mapa %>% # Añadir capa de puntos con información sobre la temperatura en los popups
  addCircleMarkers(
    lng = Datos_Filtrados$Longitude, 
    lat = Datos_Filtrados$Latitude,
    radius = 0.3, 
    color = "red",
    popup = paste("Temperatura:", Datos_Filtrados$Temperature)
  )
mapa <- mapa %>% # Añadir capa de raster
  addRasterImage(temp_predict_finca, opacity = 0.8)
mapa <- mapa %>% # Agregar título al mapa
  addControl(html = "<div id='map_title'>Predicción de la temperatura en el cultivo de Aguacate</div>", position = "topright")
mapa <- mapa %>% # Agregar minimapa
  addMiniMap(position = "bottomright")  # Ajustar la posición del minimapa según preferencias
temperature_ranges <- c(">29°C", "26°C-28°C", "24°C-25°C", "23°C-22°C", "22°<") # Definir los rangos de temperatura para la leyenda
temperature_colors <- c("#005187", "#03bb85", "#8d4925", "#7f0000", "#ffff6a") # Definir los colores para los rangos de temperatura
mapa <- mapa %>% # Agregar leyenda
  addLegend(
    position = "bottomleft", 
    colors = temperature_colors,
    labels = temperature_ranges,
    opacity = 1
  )

# Mostrar el mapa interactivo
mapa

# Realizar validación cruzada y calcular el Error Absoluto Medio (MAE)
valida <- xvalid(geodata = df_aguacate_geo, model = modelo_mco_exp)
MAE <- mean(abs(valida$error))
MAE
