# Configuración inicial
options(warn = -1, scipen = 999) 

# Cargar paquetes necesarios utilizando pacman
require(pacman)
suppressMessages(pacman::p_load(sf, ggpubr, gridExtra, cptcity, ggthemes, rnaturalearthdata, rnaturalearth, hrbrthemes, climateStability,
                                 RColorBrewer, geodata, tmap, fs, raster, terra, gstat, tidyverse, sp, readxl, leaflet, MASS, geoR, 
                                 kableExtra, DT, rasterVis, knitr, leaflet.providers))

# Cargar datos de Excel
Datos_Completos_Aguacate <- read_excel("C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°2/Unidad N°1/Datos_Completos_Aguacate.xlsx")

# Mostrar los datos en una tabla interactiva con DataTable
DT::datatable(Datos_Completos_Aguacate,
              extensions = 'FixedColumns',
              rownames = FALSE,
              options = list(
                pageLength = 10,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                autoWidth = TRUE,
                columnDefs = list(list(width = '300px', targets = c(0,7))),
                scrollX = TRUE,
                escape = TRUE)
)

# Formatear la columna de fechas y convertirla a tipo Date
Datos_Completos_Aguacate$FORMATTED_DATE_TIME <- substr(Datos_Completos_Aguacate$FORMATTED_DATE_TIME, 1, 10)
Datos_Completos_Aguacate$FORMATTED_DATE_TIME <- as.Date(Datos_Completos_Aguacate$FORMATTED_DATE_TIME, format = "%d/%m/%Y")

# Filtrar datos para una fecha específica
Datos_Filtrados <- filter(Datos_Completos_Aguacate, FORMATTED_DATE_TIME == as.Date("2020-10-01"))

# Hacer una copia de los datos filtrados
Datos_Filtrados_copy <- Datos_Filtrados

# Cargar el shapefile de la finca
finca_1 <- shapefile("~/silueta_mapa.shp")

# Definir la fórmula para la interpolación
frml <- as.formula(Temperature ~ Longitude + Latitude)

# Convertir la tabla de datos filtrados a un objeto espacial
coordinates(Datos_Filtrados_copy) <- ~Longitude + Latitude
raster::crs(Datos_Filtrados_copy) <- raster::crs(finca_1)

# Mostrar los datos filtrados en una tabla interactiva
DT::datatable(Datos_Filtrados,
              extensions = 'FixedColumns',
              rownames = FALSE,
              options = list(
                pageLength = 10,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                autoWidth = TRUE,
                columnDefs = list(list(width = '300px', targets = c(0,7))),
                scrollX = TRUE,
                escape = TRUE)
)

# Crear un gráfico de puntos con las coordenadas de los datos filtrados
ggplot(data = Datos_Filtrados, aes(x = Longitude, y = Latitude)) +
  geom_point() +
  labs(x = "Longitud", y = "Latitud", title = "Distribución de puntos de coordenadas")

# Crear un mapa interactivo con Leaflet y añadir los puntos de temperatura
leaflet() %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery) %>%
  addCircleMarkers(
    lng = Datos_Filtrados$Longitude,
    lat = Datos_Filtrados$Latitude,
    radius = 0.2,
    color = "red"
  )

# Convertir los datos filtrados a un objeto geodata
df_aguacate_geo <- as.geodata(Datos_Filtrados, coords.col = 3:2, data.col = 9)

# Crear el variograma
varg <- variogram(frml, Datos_Filtrados_copy, cloud = FALSE)
plot(varg)

# Definir los valores iniciales para el ajuste del modelo de variograma
ini.vals <- expand.grid(seq(2, 4, l = 10), seq(0.0001, 0.00025, l = 10))

# Ajustar modelos de variograma con diferentes funciones de covarianza
modelo_mco_exp <- variofit(varg, ini.vals, cov.model = "exponential")
modelo_mco_gau <- variofit(varg, ini.vals, cov.model = "gaussian") 
modelo_mco_shp <- variofit(varg, ini.vals, cov.model = "spherical")
modelo_mco_Mat <- variofit(varg, ini.vals, cov.model = "matern")

# Graficar los modelos ajustados sobre el variograma
plot(varg, pch = 16, envelop = datos_env)
lines(modelo_mco_exp, col = "blue")
lines(modelo_mco_gau, col = "red")
lines(modelo_mco_shp, col = "green")
lines(modelo_mco_Mat, col = "orange")
legend(0.0007, 7, legend = c("Gausiano", "Exponencial", "Esferico", "Matern"),
       col = c("red", "blue", "green", "orange"), lty = 1, cex = 0.8)

# Ajustar modelos de variograma usando fit.variogram
fit0 <- fit.variogram(varg, fit.ranges = TRUE, fit.sills = TRUE, vgm(psill = 4, model = 'Exp', nugget = 0)) 
fit1 <- fit.variogram(varg, fit.ranges = TRUE, fit.sills = TRUE, vgm(psill = 4, model = 'Sph', nugget = 0)) 
fit2 <- fit.variogram(varg, fit.ranges = TRUE, fit.sills = TRUE, vgm(psill = 4, model = 'Gau', nugget = 0))
fit3 <- fit.variogram(varg, fit.ranges = TRUE, fit.sills = TRUE, vgm(psill = 4, model = 'Mat', nugget = 0))

plot(varg, fit0, main = "Modelo Exponencial")
plot(varg, fit1, main = "Modelo Esférico")
plot(varg, fit2, main = "Modelo Gaussiano")
plot(varg, fit3, main = "Modelo Matern")

# Calcular y mostrar el error cuadrático medio (MSE) de los modelos
print(paste("MSE modelo Exponencial =", as.character(modelo_mco_exp$value)))
print(paste("MSE modelo Gausiano =", as.character(modelo_mco_gau$value)))
print(paste("MSE modelo Esferico =", as.character(modelo_mco_shp$value)))
print(paste("MSE modelo Matern =", as.character(modelo_mco_Mat$value))) 

# Definir los límites del área de estudio
c(min(Datos_Filtrados[, 3]),
  max(Datos_Filtrados[, 3]),
  min(Datos_Filtrados[, 2]),
  max(Datos_Filtrados[, 2]))

# Crear una cuadrícula de puntos para la interpolación
geodatos_grid <- expand.grid(lon = seq(-76.710215, -76.711799, l = 100), lat = seq(2.392101, 2.393634, l = 100))

# Ajustar la cuadrícula al área de estudio
Datos_Filtrados_copy@bbox <- finca_1@bbox

# Crear un objeto SpatialPixel a partir de la cuadrícula
grd1 <- as.data.frame(spsample(Datos_Filtrados_copy, "regular", n = 50000))
names(grd1) <- c("x1", "x2")
coordinates(grd1) <- c("x1", "x2")
gridded(grd1) <- TRUE  # Crear el objeto SpatialPixel
fullgrid(grd1) <- TRUE  # Crear el objeto SpatialGrid
raster::crs(grd1) <- raster::crs(Datos_Filtrados_copy)

# Graficar la cuadrícula y los puntos de muestreo
plot(grd1)
plot(Datos_Filtrados_copy, add = TRUE, col = 'red', pch = 16)

# Interpolación por kriging
geodatos_ko <- krige.conv(df_aguacate_geo, loc = geodatos_grid,
                          krige = krige.control(nugget = 0, trend.d = "cte",
                                                trend.l = "cte", cov.pars = c(sigmasq = 3.0186,
                                                                              phi = 0.0001)))

# Graficar los resultados de la interpolación
image(geodatos_ko, main = "kriging Predict", xlab = "East", ylab = "North")
contour(geodatos_ko, main = "kriging Predict", drawlabels = TRUE)
image(geodatos_ko, main = "kriging StDv Predicted", val = sqrt(geodatos_ko$krige.var), xlab = "East", ylab = "North")
contour(geodatos_ko, main = "kriging StDv Predict", val = sqrt(geodatos_ko$krige.var), drawlabels = TRUE)

# Convertir los resultados de la predicción a formato raster
pred <- cbind(geodatos_grid, geodatos_ko$predict)
temp_predict <- rasterFromXYZ(cbind(geodatos_grid, geodatos_ko$predict))
plot(temp_predict)
levelplot(temp_predict, par.settings = BuRdTheme)

# Convertir los errores de la predicción a formato raster
temp_error <- rasterFromXYZ(cbind(geodatos_grid, sqrt(geodatos_ko$krige.var)))
levelplot(temp_error, par.settings = BuRdTheme)

# Máscara de la finca en el raster de predicción
finca <- shapefile("~/silueta_mapa.shp")
plot(temp_predict)
plot(finca, add = TRUE)
temp_predict_finca <- mask(temp_predict, finca)
plot(temp_predict_finca)
plot(finca, add = TRUE)

# Crear el mapa utilizando la paleta de colores 'jjg_misc_temperature'
mapa_finca <- levelplot(temp_predict_finca, 
                        col.regions = cpt(pal = "jjg_misc_temperature"),  # Utilizar la paleta de colores 'jjg_misc_temperature'
                        par.settings = BuRdTheme)

# Mostrar el mapa
mapa_finca

# Kriging usando los modelos de variograma ajustados
dat.krg0 <- krige(Temperature ~ 1, Datos_Filtrados_copy, grd1, fit0)

# Convertir las predicciones de kriging a formato raster y aplicar la máscara de la finca
r0 <- raster(dat.krg0, layer = 'var1.pred')
r0 <- raster::mask(r0, finca_1)
plot(r0)
plot(Datos_Filtrados_copy, add = TRUE, col = 'red', pch = 10)

r1 <- raster(dat.krg0, layer = 'var1.var')
r1 <- raster::mask(r1, finca_1)
plot(r1)
plot(Datos_Filtrados_copy, add = TRUE, pch = 6, col = 'red')

r2 <- sqrt(raster(dat.krg0, layer = 'var1.var')) * 1.96
r2 <- raster::mask(r2, finca_1)
plot(r2)
plot(Datos_Filtrados_copy, add = TRUE, pch = 8, col = 'red')

# Reescalar los datos de incertidumbre
r2_spat <- rast(r2)
r2_rescaled <- rescale0to1(r2_spat)
plot(r2_rescaled)

# Crear un stack de los resultados de kriging
stk <- addLayer(r0, r2)
names(stk) <- c('Temperatura', 'Incertidumbre')
tbl <- rasterToPoints(stk, spatial = FALSE)
tbl <- as_tibble(tbl)

# Buscar paletas de colores adecuadas
find_cpt('temperature')
find_cpt('reds')

# Cargar el shapefile del mundo
wrld <- ne_countries(scale = 50, returnclass = 'sf')

# Graficar la superficie interpolada y la incertidumbre usando ggplot2
plot1 <- ggplot() +
  geom_tile(data = tbl, aes(x = x, y = y, fill = Temperatura)) + 
  scale_fill_gradientn(colors = cpt(pal = 'jjg_misc_temperature')) + 
  geom_sf(data = st_as_sf(finca_1), fill = NA, col = 'grey50', lwd = 0.4) +
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.2) +
  geom_sf_text(data = wrld, aes(label = admin)) +
  coord_sf(xlim = extent(finca_1)[1:2], ylim = extent(finca_1)[3:4]) + 
  ggthemes::theme_pander() +
  ggtitle(label = 'Superficie interpolada (kriging) de\ntemperatura máxima') +
  labs(x = 'Longitud', y = 'Latitud', fill = 'Temperatura (°C)') + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(5, 'line'),
        plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
        legend.title = element_text(size = 9, face = 'bold'),
        legend.key.height = unit(0.6, 'line'))

plot2 <- ggplot() +
  geom_tile(data = tbl, aes(x = x, y = y, fill = Incertidumbre)) + 
  scale_fill_gradientn(colors = rev(cpt(pal = 'ocal_reds'))) + 
  geom_sf(data = st_as_sf(finca_1), fill = NA, col = 'grey50', lwd = 0.4) +
  geom_sf(data = wrld, fill = NA, col = 'grey70', lwd = 0.2) +
  geom_sf_text(data = wrld, aes(label = admin)) +
  coord_sf(xlim = extent(finca_1)[1:2], ylim = extent(finca_1)[3:4]) + 
  ggthemes::theme_pander() +
  ggtitle(label = 'Incertidumbre de los\ndatos interpolados') +
  labs(x = 'Longitud', y = 'Latitud', fill = 'Temperatura (°C)') + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(5, 'line'),
        plot.title = element_text(size = 16, hjust = 0.5, face = 'bold'),
        legend.title = element_text(size = 9, face = 'bold'),
        legend.key.height = unit(0.6, 'line'))

# Mostrar los gráficos juntos
ggarrange(plot1, plot2, ncol = 2, nrow = 1)

# Ajustar la proyección del raster de predicción a WGS84
projection(temp_predict_finca) <- "+proj=longlat +datum=WGS84 +no_defs"

# Crear el mapa interactivo final con Leaflet
mapa <- leaflet() %>% 
  addProviderTiles(provider = providers$Esri.WorldImagery) %>%
  addCircleMarkers(
    lng = Datos_Filtrados$Longitude, 
    lat = Datos_Filtrados$Latitude,
    radius = 0.3, 
    color = "red",
    popup = paste("Temperatura:", Datos_Filtrados$Temperature)
  ) %>%
  addRasterImage(temp_predict_finca, opacity = 0.6) %>%
  addControl(html = "<div id='map_title'>Predicción de la temperatura en el cultivo de Aguacate</div>", position = "topright") %>%
  addMiniMap(position = "bottomright") %>%
  addLegend(
    position = "bottomleft", 
    colors = c("#005187","#03bb85","#8d4925","#7f0000","#ffff6a"), 
    labels = c(">29°C","26°C-28°C","24°C-25°C","23°C-22°C","22°<"),
    opacity = 1
  )

# Mostrar el mapa
mapa

# Validación cruzada y cálculo del error absoluto medio (MAE)
valida <- xvalid(geodata = df_aguacate_geo, model = modelo_mco_exp)
MAE <- mean(abs(valida$error))
MAE
