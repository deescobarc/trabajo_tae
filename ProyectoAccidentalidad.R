#Cargando librerias necesarias
library(stringr)
library(dplyr)
library(leaflet)
library(tidyverse)


# Leer archivo csv año 2014
datos_2014 = read.table("Incidentes_georreferenciados_2014.csv", header = T, sep =",", dec=".", encoding = "UTF-8")
# Leer archivo csv año 2015
datos_2015 = read.table("Incidentes_georreferenciados_2015.csv", header = T, sep =",", dec=".", encoding = "UTF-8")
# Leer archivo csv año 2016
datos_2016 = read.table("Incidentes_georreferenciados__2016.csv", header = T, sep =",", dec=".", encoding = "UTF-8")
# Leer archivo csv año 2017
datos_2017 = read.table("Incidentes_georreferenciados_2017.csv", header = T, sep =",", dec=".", encoding = "UTF-8")
# Leer archivo csv año 2018
datos_2018 = read.table("Incidentes_georreferenciados_2018.csv", header = T, sep =",", dec=".", encoding = "UTF-8")
# Unificando bases de datos
datosoriginales = rbind(datos_2014,datos_2015,datos_2016,datos_2017,datos_2018)
datos = rbind(datos_2014,datos_2015,datos_2016,datos_2017,datos_2018)

#Cambiar nombre a columna 
names(datos)[1]="X"
names(datosoriginales)[1]="X"

#Eliminar columnas noo necesarias para el análisis
datos$OBJECTID<-NULL
datos$RADICADO<-NULL
datos$HORA<-NULL
datos$DIA<-NULL
datos$PERIODO<-NULL
datos$DIRECCION<-NULL
datos$DIRECCION_ENC<-NULL
datos$CBML<-NULL
datos$TIPO_GEOCOD<-NULL
datos$DISENO<-NULL
datos$DIA_NOMBRE<-NULL
datos$MES<-NULL
datos$MES_NOMBRE<-NULL
datos$RADICADO<-NULL
datos$X_MAGNAMED<-NULL
datos$Y_MAGNAMED<-NULL
datos$LONGITUD<-NULL
datos$LATITUD<-NULL

#Eliminando filas con valores nulos y corrigiendo inconsistencias de clase
datos <- datos[datos$BARRIO!="",]
datos <- datos[datos$CLASE!="",]
datos <- datos[datos$CLASE!="Choque y Atropello",]
datos$CLASE <- str_replace_all(datos$CLASE, "Caida Ocupante","Caída de Ocupante")
datos$CLASE <- str_replace_all(datos$CLASE, "Caída Ocupante","Caída de Ocupante")
datos$CLASE <- str_replace_all(datos$CLASE, "Choque ","Choque")
datos$CLASE <- as.factor(datos$CLASE)

#Eliminando filas con valores nulos y corrigiendo inconsistencias de barrios
datos <- datos[datos$BARRIO!="0",]
datos <- datos[datos$BARRIO!="6001",]
datos <- datos[datos$BARRIO!="7001",]
datos <- datos[datos$BARRIO!="9004",]
datos <- datos[datos$BARRIO!="9086",]
datos <- datos[datos$BARRIO!="Inst",]
datos <- datos[datos$BARRIO!="AUC1",]
datos <- datos[datos$BARRIO!="AUC2",]
datos <- datos[datos$BARRIO!="Sin Nombre",]
datos <- datos[datos$BARRIO!="Laureles Estadio",]
datos <- datos[datos$BARRIO!="Corregimiento de San Antonio de Prado",]
datos <- datos[datos$BARRIO!="Corregimiento de Santa Elena",]
datos$BARRIO <- str_replace_all(datos$BARRIO, "Berlín","Berlin")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Santo Domingo Savio No.1","Santo Domingo Savio No. 1")
datos$BARRIO <- str_replace_all(datos$BARRIO, "B. Cerro  El Volador","B. Cerro El Volador")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Bomboná No.1","Bomboná No. 1")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Campo Valdés No.2","Campo Valdés No. 2")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Villa Lilliam","Villa Liliam")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Manrique Central No.1","Manrique Central No. 1")
datos$BARRIO <- str_replace_all(datos$BARRIO, "La Loma de Los Bernal","Loma de los Bernal")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Versalles No.1","Versalles No. 1")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Manrique Central No.2","Manrique Central No. 2")
datos$BARRIO <- str_replace_all(datos$BARRIO, "La Verde ","La Verde")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Aures No.2","Aures No. 2")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Asomadera No.1","Asomadera No. 1")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Moscú No.2","Moscú No. 2")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Moscú No.1","Moscú No. 1")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Versalles No.2","Versalles No. 2")
datos$BARRIO <- str_replace_all(datos$BARRIO, "Barrios de Jesús","Barrio de Jesús")
datos$BARRIO <- as.factor(datos$BARRIO)

#corrigiendo inconsistencias de comunas
datos$COMUNA <- str_replace_all(datos$COMUNA, "Alejandro Echavarría","Buenos Aires")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Alfonso López","Castilla")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Barrio Colón","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Altavista","Belén")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Andalucía","Santa Cruz")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Boston","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Campo Amor","Guayabal")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Calle Nueva","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Campo Valdés No. 1","Aranjuez")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Calasanz","La América")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Caribe","Castilla")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Cristo Rey","Guayabal")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Cerro Nutibara","Belén")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Corazón de Jesús","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "El Chagualo","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "El Nogal-Los Almendros","Belén")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Fátima","Belén")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Guayaquil","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Girardot","Castilla")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Estación Villa","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Toscana","Castilla")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Villa Carlota","El Poblado")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Universidad de Antioquia","Aranjuez")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Simón Bolívar","La América")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Santa Fé","Guayabal")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Perpetuo Socorro","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Rosales","Belén")
datos$COMUNA <- str_replace_all(datos$COMUNA, "San Pedro","Aranjuez")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Santa María de los Ángeles","El Poblado")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Villa Guadalupe","Popular")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Parque Juan Pablo II","Guayabal")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Oleoducto","Castilla")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Patio Bonito","El Poblado")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Moravia","Aranjuez")
datos$COMUNA <- str_replace_all(datos$COMUNA, "La Rosa","Santa Cruz")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Las Esmeraldas","Aranjuez")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Las Playas","Belén")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Los Mangos","Villa Hermosa")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Manila","El Poblado")
datos$COMUNA <- str_replace_all(datos$COMUNA, "La Alpujarra","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "La Floresta","La América")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Héctor Abad Gómez","Castilla")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Jesús Nazareno","La Candelaria")
datos$COMUNA <- str_replace_all(datos$COMUNA, "La Colina","Guayabal")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Miranda","Aranjuez")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Antonio Nariño","San Javier")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Campo Valdés No. 2","Manrique")
datos$COMUNA <- str_replace_all(datos$COMUNA, "El Raizal","Manrique")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Las Granjas","Manrique")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Santa Inés","Manrique")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Veinte de Julio","San Javier")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Corregimiento de San Cris","Corregimiento de San Cristóbal")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Corregimiento de San Cristóbaltóbal","Corregimiento de San Cristóbal")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Corregimiento de San Sebasti","Corregimiento de San Sebastián de Palmitas")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Corregimiento de San Sebastián de Palmitasán de Palmitas","Corregimiento de San Sebastián de Palmitas")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Corregimiento de San A","Corregimiento de San Antonio de Prado")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Corregimiento de San Antonio de Pradontonio de Prado","Corregimiento de San Antonio de Prado")
datos$COMUNA <- str_replace_all(datos$COMUNA, "Corregimiento de Belén","Corregimiento de Altavista")
datos$COMUNA <- as.factor(datos$COMUNA)

#Lectura de datos tipo fecha
datos$FECHA <- as.Date(datos$FECHA)
datosoriginales$FECHA <- as.Date(datosoriginales$FECHA)

#Agrupando los datos por barrio
barrios <- datos %>%
  group_by(BARRIO,COMUNA)%>%
  summarise(promedio=mean(datos$X))

#Agrupando los datos por gravedad herido
datos3 <- datos %>%
  group_by(COMUNA,BARRIO)%>%
  filter(GRAVEDAD=='HERIDO')%>%
  count(BARRIO, name="CANTIDAD_HERIDOS")%>%
  mutate(PROMEDIO_HERIDOS_X_MES=CANTIDAD_HERIDOS/60)

#Agrupando los datos por gravedad muertes
datos4 <- datos %>%
  group_by(COMUNA,BARRIO)%>%
  filter(GRAVEDAD=='MUERTO')%>%
  count(BARRIO, name="CANTIDAD_MUERTES")%>%
  mutate(PROMEDIO_MUERTES_X_MES=CANTIDAD_MUERTES/60)

#Agrupando los datos por gravedad solo daños
datos5 <- datos %>%
  group_by(COMUNA,BARRIO)%>%
  filter(GRAVEDAD=='SOLO DAÑOS')%>%
  count(BARRIO, name="CANTIDAD_SOLO_DAÑOS")%>%
  mutate(PROMEDIO_DAÑOS_X_MES=CANTIDAD_SOLO_DAÑOS/60)

#Uniendo las bases de datos
base_agrupamiento1 <- merge(datos3,datos4, by = c("BARRIO","COMUNA"), all=T)
base_agrupamiento <- merge(base_agrupamiento1,datos5, by = c("BARRIO","COMUNA"), all=T)

#Reemplazando los valores nulos
base_agrupamiento$CANTIDAD_HERIDOS[is.na(base_agrupamiento$CANTIDAD_HERIDOS)] <- 0
base_agrupamiento$PROMEDIO_HERIDOS_X_MES[is.na(base_agrupamiento$PROMEDIO_HERIDOS_X_MES)] <- 0
base_agrupamiento$CANTIDAD_MUERTES[is.na(base_agrupamiento$CANTIDAD_MUERTES)] <- 0
base_agrupamiento$PROMEDIO_MUERTES_X_MES[is.na(base_agrupamiento$PROMEDIO_MUERTES_X_MES)] <- 0
base_agrupamiento$CANTIDAD_SOLO_DAÑOS[is.na(base_agrupamiento$CANTIDAD_SOLO_DAÑOS)] <- 0
base_agrupamiento$PROMEDIO_DAÑOS_X_MES[is.na(base_agrupamiento$PROMEDIO_DAÑOS_X_MES)] <- 0

#Calculando variables para agrupamiento
base_agrupamiento <- base_agrupamiento%>%
  mutate(TOTAL_ACCIDENTES1 = rowSums(base_agrupamiento[,c("CANTIDAD_HERIDOS","CANTIDAD_MUERTES","CANTIDAD_SOLO_DAÑOS")]))
base_agrupamiento <- base_agrupamiento%>%
  mutate(TOTAL_ACCIDENTES = rowSums(base_agrupamiento[,c("CANTIDAD_HERIDOS","CANTIDAD_MUERTES")]))
base_agrupamiento <- base_agrupamiento%>%
  mutate( POR_HERIDOS = (TOTAL_ACCIDENTES/TOTAL_ACCIDENTES1)*100)

#Definiendo criterios de agrupamiento
a <- 900
b <- 60
base_agrupamiento <- base_agrupamiento%>%
  mutate(GRUPO =  ifelse(TOTAL_ACCIDENTES1 >= a & POR_HERIDOS >= b, 1 ,ifelse(TOTAL_ACCIDENTES1 >= a & POR_HERIDOS < b, 2, ifelse(TOTAL_ACCIDENTES1 < a & POR_HERIDOS >= b, 3, ifelse(TOTAL_ACCIDENTES1 < a & POR_HERIDOS < b, 4,"no")))))

#Crear base de datos barrios con longitud y latitud
base_barrios <- datos %>% 
  select(BARRIO, COMUNA,X,Y)%>% 
  group_by(BARRIO,COMUNA)%>% 
  summarise(X=mean(X),Y=mean(Y))

#Uniendo agrupamiento con la base de datos de barrios
datosBase <- merge(base_agrupamiento,base_barrios, by = c("BARRIO","COMUNA"), all=T)

#Convirtiendo varibale Grupo como numerica
datosBase$GRUPO <- as.numeric(datosBase$GRUPO)

#Definiendo paleta de colores para grupo
pal <- colorFactor(
  heat.colors(4), datosBase$GRUPO
)

#Presentar barrios con mas accidentes
top10Barrio <- datos%>%
              count(BARRIO)%>%
              arrange(-n)
top10Barrio <- head(top10Barrio,10)

#muestra tabla para grupos depurada
muestra <- base_agrupamiento%>%
  select(BARRIO, COMUNA, CANTIDAD_HERIDOS, PROMEDIO_HERIDOS_X_MES, CANTIDAD_MUERTES, PROMEDIO_MUERTES_X_MES,
         CANTIDAD_SOLO_DAÑOS, PROMEDIO_DAÑOS_X_MES, TOTAL_ACCIDENTES1, TOTAL_ACCIDENTES, POR_HERIDOS)

#Seleccionando variables para cluster
cluster_barrios <- muestra%>%
  select(BARRIO, TOTAL_ACCIDENTES1, POR_HERIDOS)

#escalando los datos
cluster_barrios1 <- muestra%>%
  select(TOTAL_ACCIDENTES1, POR_HERIDOS)
cluster_barrio <- scale(cluster_barrios1, center = FALSE, scale = TRUE)

#Funcion para evaluar método del codo
calcular_totalwithinss <- function(n_clusters, cluster_barrio, iter.max=1000, nstart=250){
  cluster_kmeans <- kmeans(centers = n_clusters, x=cluster_barrio, iter.max=iter.max,
  nstart=nstart)
  return(cluster_kmeans$tot.withinss)
}

total_withinss <- map_dbl(.x = 1:10,
                          .f = calcular_totalwithinss,
                          cluster_barrio=cluster_barrio)

#?barrios por grupo
grupoBarrios <- datosBase %>%
  count(GRUPO, name="CANTIDAD_BARRIOS")
