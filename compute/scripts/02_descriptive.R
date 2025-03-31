#---- LIBRARIES ----

library(ggplot2)
library(tidyverse)
library(sf)
library(stringr)

#---- READ DATA ----

dataSP <- st_read("compute/data/dataSP.shp")
dataSP2 <- st_read("compute/data/dataSP2.shp")

#---- RESUMEN DE DATOS / VISUALIZACIONES DESCRIPTIVAS ----

categ <- as.data.frame(table(dataSP$CATEGOR))
categ$Var1 <- factor(categ$Var1, levels = categ$Var1[order(-categ$Freq)])

# Crear el gráfico de barras con porcentajes en las barras
ggplot(categ, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +  # Crear las barras
  labs(x = "Categoría", y = "Frecuencia") +  # Etiquetas de los ejes
  ggtitle("") +  # Título del gráfico
  theme_minimal() +  # Estilo del gráfico
  scale_y_continuous(
    labels = scales::comma_format(scale = 1),
    breaks = seq(0, max(categ$Freq), by = 50000)  # Define los valores de los breaks
  ) +
  geom_text(
    aes(label = scales::percent(Freq / sum(Freq))),
    vjust = -0.5,   # Ajusta la posición vertical del texto
    size = 3)       # Tamaño del texto


# se agurpan los datos por CCZ y CATEGORIA, demora un poco
resumen_datos <- dataSP %>% select(-geometry) %>%
  group_by(CCZ, CATEGOR) %>%
  summarize(Cantidad = n()) %>%
  ungroup()

resumen_datos <- as.data.frame(resumen_datos)

# se grafica la distribucion de TDS por ccz
ggplot(resumen_datos, aes(x = factor(CCZ), y = Cantidad, fill = CATEGOR)) +
  geom_bar(stat = "identity") +
  labs(x = "Centros Comunales Zonales", y = "Cantidad") +
  scale_fill_discrete() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# cantidades en ccz 9
#sum(resumen_datos$Cantidad[resumen_datos$CCZ=='9'])
#sum(resumen_datos$Cantidad[resumen_datos$CCZ=='1'&resumen_datos$CATEGORIA=='DOMICILIARIO'])
#sum(resumen_datos$Cantidad[resumen_datos$CCZ=='1'&resumen_datos$CATEGORIA=='COMERCIAL/INDUSTRIAL'])
#sum(resumen_datos$Cantidad[resumen_datos$CATEGORIA=='GUBERNAMENTAL'])
#sum(resumen_datos$Cantidad[resumen_datos$CATEGORIA=='ARTICULO 91'])



#agrupamos la cantidad de TDS por CCZ
#ts_por_ccz <- dataSP %>% group_by(dataSP$CCZ) %>% transmute(cantidad_cuentas=n())

ts_por_ccz <- as.data.frame(table(dataSP$CCZ)) 
ts_por_ccz <- rename(ts_por_ccz, ccz = Var1, cantTDS = Freq)

ts_cortadas_por_ccz <- dataSP %>% group_by(dataSP$CCZ) %>% mutate(cantidad_cuentas=n(),cortadas=sum(TS_CORT=='1'))

ts_cortadas_por_ccz <- as.data.frame(matrix(unique(c(ts_cortadas_por_ccz$CCZ , ts_cortadas_por_ccz$cortadas)), ncol = 2))
ts_cortadas_por_ccz <- rename(ts_cortadas_por_ccz, ccz = V1, cantTDScortadas = V2)

resumen_ccz <- merge(ts_por_ccz , ts_cortadas_por_ccz , by=c('ccz','ccz'))

# trabajamos con el objeto ccz que tiene la geometria de cada comunal

ccz$ZONA_LEGAL <- str_replace(ccz$ZONA_LEGAL, "CCZ0", "") # funcion de la libreria stringr
ccz$ZONA_LEGAL <- str_replace(ccz$ZONA_LEGAL, "CCZ", "")

resumen_ccz <- resumen_ccz %>% mutate(proporcion = round((cantTDScortadas/cantTDS)*100,2))

ccz$ZONA_LEGAL <- as.integer(ccz$ZONA_LEGAL)
resumen_ccz$ccz <- as.integer(resumen_ccz$ccz)
resumen_ccz$cantTDS <- as.numeric(resumen_ccz$cantTDS)

cczSP <- left_join(ccz,resumen_ccz,by=c("ZONA_LEGAL"="ccz"))


tmap_mode("view")
# tds por ccz
tm_shape(cczSP)+
  tm_polygons(col="cantTDS",title="",border.col="red",alpha =.5)+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_text("ZONA_LEGAL", size = 1.5) #+
  #tm_basemap('Esri.WorldImagery')

# proporicion de tds cortadas por ccz
tm_shape(cczSP)+
  tm_polygons(col="proporcion",title="",border.col="red",alpha =.5)+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_text("ZONA_LEGAL", size = 1.5)#+
  #tm_basemap('Esri.WorldImagery')

# cuentas activas por ccz
ts_activas_por_ccz <- dataSP %>% group_by(dataSP$CCZ) %>% mutate(cantidad_cuentas=n(),activas=sum(TS_CORT=='0')) 

ts_activas_por_ccz <- as.data.frame(matrix(unique(c(ts_activas_por_ccz$CCZ , ts_activas_por_ccz$activas)), ncol = 2))
ts_activas_por_ccz <- rename(ts_activas_por_ccz, ccz = V1, cantTDSactivas = V2)

resumen_ccz2 <- merge(ts_por_ccz , ts_activas_por_ccz , by=c('ccz','ccz'))

resumen_ccz2 <- resumen_ccz2 %>% mutate(proporcion = round((cantTDSactivas/cantTDS)*100,2))

resumen_ccz2$ccz <- as.integer(resumen_ccz$ccz)
resumen_ccz2$cantTDS <- as.numeric(resumen_ccz$cantTDS)

cczSP2 <- left_join(ccz,resumen_ccz2,by=c("ZONA_LEGAL"="ccz"))

# cuentas activas por ccz
tm_shape(cczSP2)+
  tm_polygons(col="proporcion",title="",border.col="red",alpha =.5)+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_text("ZONA_LEGAL", size = 1.5)

ts_activas_por_ccz <- dataSP %>% select(-geometry) %>% group_by(CCZ) %>% mutate(cantidad_cuentas = n(),
    activas = sum(ifelse(TS_CORT == '0' & CANT_BI <= 6, 1, 0)))

ts_activas_por_ccz <- as.data.frame(matrix(unique(c(ts_activas_por_ccz$CCZ , ts_activas_por_ccz$activas)), ncol = 2))
ts_activas_por_ccz <- rename(ts_activas_por_ccz, ccz = V1, cantTDSactivas = V2)
# sum(ts_activas_por_ccz$cantTDSactivas) # 195248
resumen_ccz2 <- merge(ts_por_ccz , ts_activas_por_ccz , by=c('ccz','ccz'))

resumen_ccz2 <- resumen_ccz2 %>% mutate(proporcion = round(100-(cantTDSactivas/cantTDS)*100,2))

resumen_ccz2$ccz <- as.integer(resumen_ccz$ccz)
resumen_ccz2$cantTDS <- as.numeric(resumen_ccz$cantTDS)

cczSP3 <- left_join(ccz,resumen_ccz2,by=c("ZONA_LEGAL"="ccz"))

# cuentas activas y con menos de un anio de deuda por ccz
tm_shape(cczSP3)+
  tm_polygons(col="proporcion",title="",border.col="red",alpha =.5)+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_text("ZONA_LEGAL", size = 1.5)

tm_shape(cczSP3) +
  tm_polygons(col = "proporcion", title = "", border.col = "red", alpha = 0.5) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_shape(cczSP3) +
  tm_text("proporcion", size = 1, col = "black", bg.color = "white")




#---- FILTRADO DE CCZ1 ----

dataSP2_1 <- dataSP2 %>% filter(CCZ == '1') %>% group_by(PADRON) %>% mutate(cantTDS=n())

st_write(dataSP2_1, "compute/data/ccz1.shp")

# cuenta por padron del ccz1
tmap_mode("view")

tm_shape(dataSP2_1)+
  tm_polygons(col="cantTDS", id="PADRON", title='', border.col="red",alpha =.5 , breaks=c(1,2,3,6,21,max(dataSP2_1$cantTDS)),style="fixed")+
  tm_scale_bar(position=c("left", "bottom"))

#por defecto el numero que muestra es el padron, cuando quiero especificar que sea padron demora en cargar y cancele
#+ tm_text(text = 'PADRON') #debe ser porque en tm_text le agrega un texto a cada padron
#  tm_basemap('Esri.WorldImagery')


# cantidad de bimestres impagos por padron del ccz1
dataSP2_2 <- dataSP2 %>% filter(CCZ == '1') %>% group_by(PADRON) %>% mutate(promedio=mean(CANT_BI)) %>% ungroup()

tm_shape(dataSP2_2)+
  tm_polygons(col="promedio", id="PADRON", title='', border.col="red",alpha =.5 , breaks=c(0,1,3,6,60,130),style="fixed")+
  tm_scale_bar(position=c("left", "bottom"))










# NO CORRER ESTO
'
# agrupamos datos para ver la distribucion de CANTIDAD_BIM_IMPAGOS
resumen_categorias <- dataSP %>% select(-geometry) %>%
  group_by(CANT_BIM_IMPAGOS) %>%
  summarize(Repeticiones = n()) %>%
  arrange((CANT_BIM_IMPAGOS))

resumen_categorias <- resumen_categorias %>%
  mutate(CANT_BIM_IMPAGOS_Agrupado = case_when(
    CANT_BIM_IMPAGOS %% 1 == 0.5 ~ CANT_BIM_IMPAGOS - 0.5,
    TRUE ~ CANT_BIM_IMPAGOS
  )) %>%
  group_by(CANT_BIM_IMPAGOS_Agrupado) %>%
  summarize(Repeticiones = sum(Repeticiones)) %>%
  ungroup() %>%
  arrange(CANT_BIM_IMPAGOS_Agrupado)

resumen_categorias <- resumen_categorias %>% slice(-1,-2)

ggplot(resumen_categorias, aes(x = CANT_BIM_IMPAGOS_Agrupado, y = Repeticiones)) +
  geom_histogram(stat = "identity") +
  labs(x = "Categoría de CANT_BIM_IMPAGOS", y = "Cantidad de Repeticiones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) + xlim(0,50)
'