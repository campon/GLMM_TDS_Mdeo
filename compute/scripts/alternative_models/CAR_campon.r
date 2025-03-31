library(sf)
library(brms)
library(tmap)
library(spdep)
library(caret)

set.seed(2) # para reproducibilidad

#create train and test data
splitIndex <- createDataPartition(sin_generar$BIM6, p = 1, list = FALSE)
trainDataList <- sin_generar[splitIndex, ]
testDataList <- sin_generar[-splitIndex, ]

# un mapita con los poligonos de las tarifas sin historial de pago
tm_shape(sin_generar) + 
  tm_polygons(col='BIM6')

# voy a crear vecinos a partir de una distanciia D, pero como
# no se cual deberia ser un minimo valor de D, hago esto
centro <- st_centroid(st_geometry(sin_generar))
nb1 <- knn2nb(knearneigh(centro, k = 1))
dist1 <- nbdists(nb1, centro)
summary(unlist(dist1))
# ta, uso 1500m

# construimos vecinos
di <- dnearneigh(centro,0,1500)
# y construimos matrizz de conectividad PARA TARIFAS (carlitos)
Wobs <- nb2listw(di)
# creo que por defecto esta es la estandarizada por filas

# genero variable 0-1 (no me acuerdo xq)
sin_generar$pago<-as.numeric(sin_generar$BIM6)-1

# hay autocorrelacion espacial?
moran.test(x = sin_generar$pago, listw = Wobs)
# parece que si, no se si es 'mucha'... capaz que habria que indagar sobre esto






# leo un mapa con los CCZ de montevideo
ccz <- st_read('ine_ccz_mvd.shp')
# y le asigno el crs de los carlitos
st_crs(ccz)<- st_crs(sin_generar)

# un mapa (feo, arreglalo si quieres)
tmap_mode("view")
tm_shape(sin_generar) + 
  tm_polygons(col='BIM6') + 
  tm_shape(ccz) + 
  tm_polygons(col='navy',alpha=0.1) +
  tm_text("CCZ", size = 1.5)

# creo una matriz de conectividad PARA CCZ's
Wccz <- nb2listw(poly2nb(ccz))

# me quedo con la matriz
w    <- listw2mat(Wccz)
# y parece que a brms solo le gusta con 0's y 1' (la podría haber usando con style=B' antes, no?)
w[w>0]<-1

# modelo espacial
mod1  <- brm(pago ~ ANTIGUEDAD + PROP_BI + car(w, gr=CCZ, type = 'escar'), data=sin_generar, family=bernoulli(), data2=list(w=w))
summary(mod1)

# modelo espacial intrinseco
mod2  <- brm(pago ~ ANTIGUEDAD + PROP_BI + car(w, gr=CCZ, type = 'esicar'), data=sin_generar, family=bernoulli(), data2=list(w=w))
summary(mod2)

# modelo no espacial
mod0 <- brm(pago ~ ANTIGUEDAD + PROP_BI, data=sin_generar, family=bernoulli())
summary(mod0)

# modelo no espacial glm
mod00 <- glm(pago ~ ANTIGUEDAD + PROP_BI, data=sin_generar, family = binomial(link = 'logit'))
summary(mod00)

# cual gana?
loo::waic(mod0,mod1,mod2)

# valores de los efectos aleatorios de los CCZ (puede servir para pintar un mapa)
apply(ed$dpars$mu$ac$rcar,2,mean)

# funcion para obtener residuos de devianza
dres <- function(brms_mod){
  #e <- residuals(brms_mod)[,1]
  #f <- fitted(brms_mod)[,1]
  #y <- brms_mod$data$pago
  e <- residuals(brms_mod)
  f <- fitted(brms_mod)
  y <- sin_generar$pago
  devr <- sign(e)*sqrt(-2*(y*log(f)+(1-y)*log(1-f)))
  return(devr)
}

# extraemos residuos de devianza
# extraer residuos de devianza de mod00


sin_generar$dres00 <- dres(mod00)
sin_generar$dres0 <- dres(mod0)[,1]
sin_generar$dres1 <- dres(mod1)[,1]
sin_generar$dres2 <- dres(mod2)[,1]

moran.test(x = sin_generar$dres00, listw = Wobs)
moran.test(x = sin_generar$dres0, listw = Wobs)
moran.test(x = sin_generar$dres1, listw = Wobs)
moran.test(x = sin_generar$dres2, listw = Wobs)
# chiche!

# y las predicciones
sin_generar$f0 <- fitted(mod0)[,1]
sin_generar$f1 <- fitted(mod1)[,1]
sin_generar$f2 <- fitted(mod2)[,1]

with(sin_generar, table(BIM6, ifelse(f0>0.5,1,0))) -> t0
with(sin_generar, table(BIM6, ifelse(f1>0.5,1,0))) -> t1
with(sin_generar, table(BIM6, ifelse(f2>0.5,1,0))) -> t2

round(prop.table(t0,1),3)
1 - sum(diag(t0))/sum(t0)

round(prop.table(t1,1),3)
1 - sum(diag(t1))/sum(t1)

round(prop.table(t2,1),3)
1 - sum(diag(t2))/sum(t2)

# no sera una maravilla, pero mejora. FIN DEL TFG


# predictions
# Hacer predicciones en el conjunto de datos de prueba
predicciones_mod1 <- posterior_predict(mod1, newdata = testDataList)

# Si quieres obtener las predicciones promedio o mediana puedes usar:
predicciones_promedio_mod1 <- colMeans(predicciones_mod1)

with(testDataList, table(BIM6, ifelse(predicciones_promedio_mod1>0.5,1,0))) -> results

round(prop.table(results,1),3)
1 - sum(diag(results))/sum(results)

# car
ed <- prepare_predictions(mod1)

apply(ed$dpars$mu$ac$rcar,2,mean)

#icar
ed2 <- prepare_predictions(mod2)

apply(ed2$dpars$mu$ac$rcar,2,mean)









# AUTOCORRELACION ESPACIAL BY CCZ

# agrugapar observaciones en sin_genearr por ccz, y calcular la media de BIM6
sin_generar$BIM6 <- as.numeric(sin_generar$BIM6) -1 
sin_generar$CCZ <- as.factor(sin_generar$CCZ)


library(dplyr)


# agrupar
sin_generar %>% 
  group_by(CCZ) %>% 
  summarise(mean_BIM6 = mean(BIM6, na.rm = TRUE), total = n()) -> mean_BIM6

sin_generar %>% 
  group_by(CCZ) %>% 
  summarise(mean_dres0 = mean(dres00, na.rm = TRUE), total = n()) -> mean_dres0

sin_generar %>% 
  group_by(CCZ) %>% 
  summarise(median_dres0 = median(dres00, na.rm = TRUE), total = n()) -> median_dres0

sin_generar %>% 
  group_by(CCZ) %>% 
  summarise(mean_dres1 = mean(dres1, na.rm = TRUE), total = n()) -> mean_dres1

sin_generar %>% 
  group_by(CCZ) %>% 
  summarise(median_dres1 = median(dres1, na.rm = TRUE), total = n()) -> median_dres1

sin_generar %>% 
  group_by(CCZ) %>% 
  summarise(mean_dres2 = mean(dres2, na.rm = TRUE), total = n()) -> mean_dres2

sin_generar %>% 
  group_by(CCZ) %>% 
  summarise(median_dres2 = median(dres2, na.rm = TRUE), total = n()) -> median_dres2


# crear variables mean_bim6 y mean_dres0 en un mismo objeto
mean_BIM6 <- as.data.frame(mean_BIM6)[,c('CCZ','mean_BIM6')]
mean_dres0 <- as.data.frame(mean_dres0)[,c('CCZ','mean_dres0')]

# drop geometry meanBIM6

ccz <- merge(ccz, mean_BIM6, by.x = 'CCZ', by.y = 'CCZ')
ccz <- merge(ccz, mean_dres0, by.x = 'CCZ', by.y = 'CCZ')

# tmap con el promedio de BIM6 por CCZ
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='mean_dres0') + 
  tm_text("CCZ", size = 1.5)

# crear matriz de conectividad
Wccz <- nb2listw(poly2nb(ccz))

# hacer moran test de los CCZ
moran.test(x = ccz$mean_BIM6, listw = Wccz)

# no funciona

#PERO SI FUNCIONA CON LA MEDIANA!!!!!!!

moran.test(mean_dres0$mean_dres0, listw = Wccz)
# p-valor 0.63 no hay dependencia espacial


moran.test(mean_BIM6$mean_BIM6, listw = Wccz) # se rechaza H0

moran.test(mean_dres0$mean_dres0, listw = Wccz) # se rechaza H0
moran.test(median_dres0$median_dres0, listw = Wccz) # se rechaza H0

moran.test(mean_dres1$mean_dres1, listw = Wccz) # se rechaza H0
moran.test(median_dres1$median_dres1, listw = Wccz) # NO se rechaza H0

moran.test(mean_dres2$mean_dres2, listw = Wccz) # casi no se rechaza H0
moran.test(median_dres2$median_dres2, listw = Wccz) # NO se rechaza H0








# imagenes ilustrativas de los criterios de vecindad

# crear un plot de los ccz con la conectividad binaria de cada observacion
centroids <- st_geometry(ccz) |> st_centroid()
nb_q <- poly2nb(ccz)
nb_lines <- nb2lines(nb = nb_q, coords = centroids)

library(tidyverse)
df_points_within <- ccz %>% 
  mutate(point_within = st_point_on_surface(geometry)) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  st_as_sf()

tmap_mode("view")
tm_shape(ccz) +
  tm_polygons(col = "coral", border.col = "grey75") +
  #tm_shape(df_points_within) +
  tm_text("CCZ", size = 1, xmod=2.5, ymod=0) +
  tm_shape(nb_lines) + 
  tm_lines(alpha = .5, col = "gold") +
  tm_shape(centroids) +
  tm_dots(size = .1, alpha = .5) # add tm text names of each polygon (CCZ)


obs <- sin_generar[sample(1:nrow(sin_generar),1),]

tmap_mode("view")
tm_shape(obs) + 
  tm_polygons(col='BIM6') + 
  tm_shape(st_centroid(st_geometry(obs))) + 
  tm_dots() + 
  tm_shape(st_buffer(st_centroid(st_geometry(obs)),1500)) + 
  tm_borders() + 
  tm_shape(sin_generar[di[[as.numeric(row.names(obs))]],]) + 
  tm_polygons(col='red')  + 
  tm_markers()
# 916 x 688







