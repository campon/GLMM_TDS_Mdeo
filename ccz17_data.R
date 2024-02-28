# data ccz 17

library(spdep)
library(tidyverse)

# cargamos los datos
dataSP3 <- st_read("TFG/archivos/dataSP3.shp")

# construyo la variable antiguedad a partir de F_APERTURA
dataSP3 <- dataSP3  %>% mutate(ANTIGUEDAD = (as.Date("2022-12-23") - F_APERT)/365.25)

# transformacion de variables
dataSP3$CATEGOR <- as.factor(dataSP3$CATEGOR)

dataSP3$BIM6 <- as.numeric(dataSP3$BIM6) 

dataSP3$BIM5 <- as.factor(dataSP3$BIM5)

dataSP3$TS_CORT <- as.factor(dataSP3$TS_CORT)

dataSP3$CCZ <- as.factor(dataSP3$CCZ)

#ccz 17

ccz17 <- dataSP3 %>% filter(CCZ == '17')

# Calcular los centroides de los polígonos
centroides17 <- st_centroid(ccz17$geometry)

# Obtener las coordenadas de los centroides
coordenadas17 <- st_coordinates(centroides17)

W50.17 <- dnearneigh(coordenadas17, d1 = 0, d2 = 50, longlat = FALSE)

data <- list(N = nrow(ccz17),  
             y = ccz17$BIM6,
             E = ccz17$CANT_UN, 
             x = ccz17$PROP_BI
);

#summary(data)


