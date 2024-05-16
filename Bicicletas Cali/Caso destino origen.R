### Caso Encuesta origen - destino

# Cargue base de datos.

## Base de datos comunas
library(sp)
library(raster)
library(sf)
library(tidyverse)
library(readxl)
library(gridExtra)
library(janitor)
library(tmap)
library(kableExtra)
library(DT)

# Cargue Shapefile Comunas

comunas <- shapefile("C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°1/Unidad N°1/Casos/cali/Comunas.shp")
comunas@data # ver contenido de comunas

lbls<- as.character(comunas$comuna) # Se extrae la columna comuna y se conviert en caracter
spl = list('sp.text', coordinates(comunas), lbls, cex=.6)
spl[[2]][7,]=spl[[2]][7,]+0.005

# Creación mapa comunas de cali
g1=spplot(comunas[,3], col.regions= heat.colors(20,.95,.4), sp.layout = spl, main = "Comunas de Cali")
g1
# Cargue base de datos Origen-Destino
Enucuesta_BD <- read_excel("C:/Users/juanr/Downloads/rigenDestino.xlsx")
str(Enucuesta_BD)
## conversion de nombres. Limpiamos los nombres de la encuenta_Bd y se selección las columnas ("comuna_origen", "comuna_destino" y "tipo_de_vehiculo")
Enucuesta_BD_2 <- Enucuesta_BD %>% clean_names()%>% dplyr::select(c("comuna_origen","comuna_destino", "tipo_de_vehiculo"))
## Sefiltran las columnas comuna_origen y comuna_destino y se extrae los datos de estac comunas que son diferente a "Fuera de Cali" y a "0"
Enucuesta_BD_2<-Enucuesta_BD_2[Enucuesta_BD_2$comuna_origen!="Fuera de Cali" & Enucuesta_BD_2$comuna_destino!= "Fuera de Cali" & Enucuesta_BD_2$comuna_origen!= "0" & Enucuesta_BD_2$comuna_destino!= "0", ]
str(Enucuesta_BD_2)
## Amboas columnas se pasan a tipo numerico porque estan en tipo caracter
Enucuesta_BD_2$comuna_origen<-as.numeric(Enucuesta_BD_2$comuna_origen)
Enucuesta_BD_2$comuna_destino<-as.numeric(Enucuesta_BD_2$comuna_destino)
str(Enucuesta_BD_2)
# Leer el archivo shapefile
comunas_1 <- st_read("C:/Users/juanr/OneDrive - PUJ Cali/U Javeriana/Segundo Semestre/Anál. de Infor. Geogr. y Espa/Módulo N°1/Unidad N°1/Casos/cali/Comunas.shp")


# se realiza la suma de los vehucilos por comuna 
viajes_todos_vehiculos <- Enucuesta_BD_2 %>%
  group_by(comuna_origen) %>%
  summarise(cantidad_viajes = n()) 
# Creación de la tabla
datatable(viajes_todos_vehiculos, 
          caption = "Cantidad de viajes desde cada comuna de origen",
          options = list(pageLength = 11))  # Define cuántas filas mostrar por página
# Unir los datos de suma de vehículos a las comunas
comunas_origen <- left_join(comunas_1, viajes_todos_vehiculos, by = c("comuna" = "comuna_origen"))

# Definir una paleta personalizada más clara
mi_paleta <- colorRampPalette(c("#f7feae","#b7e6a5","#7ccba2","#46aea0","#089099","#00718b","#045275"))(7)
## creación mapa origen 
# Cambiar color del mapa coroplético
tm_shape(comunas_origen) +
  tm_polygons("cantidad_viajes", style = "jenks", 
              palette = mi_paleta, title = "Cantidad de viajes") +
    # Invertir la leyenda
  tm_layout(legend.position = c("right", "bottom"), 
            title = "¿De dónde se desplazan la mayoría de personas?") +
    # Añadir etiquetas de las comunas en negrita y color negro
  tm_text("nombre", size = 0.6, col = "black", fontface = "bold")

## Mapa  Viajes Origen Bicicleta.

# Calcular la suma de vehículos tipo 1 (Bicicletas) según la comuna de origen
sumas_comuna_origen <- Enucuesta_BD_2 %>%
  filter(tipo_de_vehiculo == 1) %>%
  group_by(comuna_origen) %>%
  summarise(total_vehiculos_tipo_1 = n())

# Unir los datos de suma de vehículos a las comunas
comunas_1 <- left_join(comunas_1, sumas_comuna_origen, by = c("comuna" = "comuna_origen"))
# Definir una paleta personalizada más clara
mi_paleta <- colorRampPalette(c("#ede5cf","#e0c2a2","#d39c83","#c1766f","#a65461","#813753","#541f3f"))(7)
# Cambiar color del mapa coroplético
mapa_1_Bicicletas<-tm_shape(comunas_1) +
  tm_polygons("total_vehiculos_tipo_1", style = "jenks",palette = mi_paleta, title = "Cantidad de viajes de bicicleta") +
  tm_layout(legend.position = c("right", "bottom"), 
            title = "¿De dónde se realizan la mayoría de los viajes hechos en bicicletas? ") +
  tm_text("nombre", size = 0.6, col = "black", fontface = "bold")
mapa_1_Bicicletas

## Mapa Viajes Origen Motos.

# Calcular la suma de vehículos tipo 2 (Motos) según la comuna de origen
sumas_comuna_origen_moto <- Enucuesta_BD_2 %>%
  filter(tipo_de_vehiculo == 2) %>%
  group_by(comuna_origen) %>%
  summarise(total_vehiculos_tipo_2 = n())

# Unir los datos de suma de vehículos a las comunas
comunas_2 <- left_join(comunas_1, sumas_comuna_origen_moto, by = c("comuna" = "comuna_origen"))

#ede5cf,#e0c2a2,#d39c83,#c1766f,#a65461,#813753,#541f3f
# Definir una paleta personalizada más clara
mi_paleta <- colorRampPalette(c("#ede5cf","#e0c2a2","#d39c83","#c1766f","#a65461","#813753","#541f3f"))(7)

# Cambiar color del mapa coroplético
mapa_2_Motos<-tm_shape(comunas_2) +
  tm_polygons("total_vehiculos_tipo_2", style = "jenks", 
              palette = mi_paleta, title = "Cantidad de viajes en Moto") +
  
  # Invertir la leyenda
  tm_layout(legend.position = c("right", "bottom"), 
            title = "¿De dónde se realizan la mayoría de los viajes hechos en Moto? ") +
  
  # Añadir etiquetas de las comunas en negrita y color negro
  tm_text("nombre", size = 0.6, col = "black", fontface = "bold")
mapa_2_Motos

## Mapa Viajes Origen Carros.

# Calcular la suma de vehículos tipo 3 (Carros) según la comuna de origen
sumas_comuna_origen_carros <- Enucuesta_BD_2 %>%
  filter(tipo_de_vehiculo == 3) %>%
  group_by(comuna_origen) %>%
  summarise(total_vehiculos_tipo_3 = n())
# Unir los datos de suma de vehículos a las comunas
comunas_3 <- left_join(comunas_1, sumas_comuna_origen_carros, by = c("comuna" = "comuna_origen"))
# Definir una paleta personalizada más clara
mi_paleta <- colorRampPalette(c("#ede5cf","#e0c2a2","#d39c83","#c1766f","#a65461","#813753","#541f3f"))(7)
# Cambiar color del mapa coroplético
mapa_3_Carros<-tm_shape(comunas_3) +
  tm_polygons("total_vehiculos_tipo_3", style = "jenks", 
              palette = mi_paleta, title = "Cantidad de viajes en Carro") +
    # Invertir la leyenda
  tm_layout(legend.position = c("right", "bottom"), 
            title = "¿De dónde se realizan la mayoría de los viajes hechos en Carro? ") +
    # Añadir etiquetas de las comunas en negrita y color negro
  tm_text("nombre", size = 0.6, col = "black", fontface = "bold")
mapa_3_Carros


# Viajes - Destinos.

## creación de base de datos por la sumna de la cantidad de vehúculos según comuna de destino
viajes_todos_vehiculos_destino <- Enucuesta_BD_2 %>%
  group_by(comuna_destino) %>%
  summarise(cantidad_viajes = n()) 
## Tabla de lo anterior.
datatable(viajes_todos_vehiculos_destino, 
          caption = "Cantidad de viajes desde cada comuna de Destino",
          options = list(pageLength = 11))  # Define cuántas filas mostrar por página
comunas_destino <- left_join(comunas_1, viajes_todos_vehiculos_destino, by = c("comuna" = "comuna_destino"))

## Mapa de los vijaes según destino 
# Definir una paleta personalizada más clara
mi_paleta <- colorRampPalette(c("#fcde9c","#faa476","#f0746e","#e34f6f","#dc3977","#b9257a","#7c1d6f"))(7)
# Cambiar color del mapa coroplético
tm_shape(comunas_destino) +
  tm_polygons("cantidad_viajes", style = "jenks", 
              palette = mi_paleta, title = "Cantidad de viajes") +
  # Invertir la leyenda
  tm_layout(legend.position = c("right", "bottom"), 
            title = "¿A dónde llegan la mayoría de personas?") +
  # Añadir etiquetas de las comunas en negrita y color negro
  tm_text("nombre", size = 0.6, col = "black", fontface = "bold")


##  Mapa Viajes Origen Bicicletas.
# Calcular la suma de vehículos tipo 1 según la comuna de origen
sumas_comuna_destino <- Enucuesta_BD_2 %>%
  filter(tipo_de_vehiculo == 1) %>%
  group_by(comuna_destino) %>%
  summarise(total_vehiculos_tipo = n())

# Unir los datos de suma de vehículos a las comunas
comunas_1_Destino <- left_join(comunas_1, sumas_comuna_destino, by = c("comuna" = "comuna_destino"))


# Definir una paleta personalizada más clara
mi_paleta <- colorRampPalette(c("#ffffb2","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"))(7)

# Cambiar color del mapa coroplético
mapa_Bicicletas_destino<-tm_shape(comunas_1_Destino) +
  tm_polygons("total_vehiculos_tipo", style = "jenks", 
              palette = mi_paleta, title = "Cantidad de viajes de bicicleta") +
  
  # Invertir la leyenda
  tm_layout(legend.position = c("right", "bottom"), 
            title = "¿A dónde llegan la mayoría de personas que viajan en bicicletas? ") +
  
  # Añadir etiquetas de las comunas en negrita y color negro
  tm_text("nombre", size = 0.6, col = "black", fontface = "bold")
mapa_Bicicletas_destino


## Mapa Viajes Origen Motos.
# Calcular la suma de vehículos tipo 2 según la comuna de origen
sumas_comuna_destino_Motos <- Enucuesta_BD_2 %>%
  filter(tipo_de_vehiculo == 2) %>%
  group_by(comuna_destino) %>%
  summarise(total_vehiculos_tipo = n())

# Unir los datos de suma de vehículos a las comunas
comunas_1_Destino_motos <- left_join(comunas_1, sumas_comuna_destino_Motos, by = c("comuna" = "comuna_destino"))


# Definir una paleta personalizada más clara
mi_paleta <- colorRampPalette(c("#ffffb2","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"))(7)

# Cambiar color del mapa coroplético
mapa_Motos_destino<-tm_shape(comunas_1_Destino_motos) +
  tm_polygons("total_vehiculos_tipo", style = "jenks", 
              palette = mi_paleta, title = "Cantidad de viajes en moto") +
  
  # Invertir la leyenda
  tm_layout(legend.position = c("right", "bottom"), 
            title = "¿A dónde llegan la mayoría de personas que viajan en Moto? ") +
  
  # Añadir etiquetas de las comunas en negrita y color negro
  tm_text("nombre", size = 0.6, col = "black", fontface = "bold")
mapa_Motos_destino



## Mapa Viajes Origen Carros.
# Calcular la suma de vehículos tipo 3 según la comuna de origen
sumas_comuna_destino_Carro <- Enucuesta_BD_2 %>%
  filter(tipo_de_vehiculo == 3) %>%
  group_by(comuna_destino) %>%
  summarise(total_vehiculos_tipo = n())
# Unir los datos de suma de vehículos a las comunas
comunas_1_Destino_carro <- left_join(comunas_1, sumas_comuna_destino_Carro, by = c("comuna" = "comuna_destino"))
# Definir una paleta personalizada más clara
mi_paleta <- colorRampPalette(c("#ffffb2","#fed976","#feb24c","#fd8d3c","#fc4e2a","#e31a1c","#b10026"))(7)
# Cambiar color del mapa coroplético
mapa_3_Carros_destino<-tm_shape(comunas_1_Destino_carro) +
  tm_polygons("total_vehiculos_tipo", style = "jenks", 
              palette = mi_paleta, title = "Cantidad de viajes en Carro") +
  # Invertir la leyenda
  tm_layout(legend.position = c("right", "bottom"), 
            title = "¿A dónde llegan la mayoría de personas que viajan en Carro?") +
  # Añadir etiquetas de las comunas en negrita y color negro
  tm_text("nombre", size = 0.6, col = "black", fontface = "bold")
mapa_3_Carros_destino



### Vizualización de los mapas de origen y destipo por vehículo
tmap_arrange(mapa_1_Bicicletas, mapa_Bicicletas_destino,
             mapa_2_Motos, mapa_Motos_destino,
             mapa_3_Carros, mapa_3_Carros_destino,nrow = 3
)