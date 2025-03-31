# CAR (spatial conditional autoreggresive)
# definir M (matriz de ady) por distancia 1 cuadra o 50 mas cercanos. LISTO - VISTO CON FERNANDO

# transformar CANT_BIM como / ANTIGUEDAD en bim convirtiendola en una proporcion para permite tener en cuenta 
# la antiguedad, pq si tiene 4 = CANT_BIM no sabemos su antiguedad si es en 4 años o 100 años. LISTO - VISTO CON FERNANDO

# construir facet con los 4 coeficeientes de los OR
# y otra fila con los del modelo completo

# probar los residuos de devianza para el moran test y la sensibilidad con la 
# distancia de los vecinos en dnearheigh. LISTO - VISTO CON FERNANDO

library(spdep)
library(tidyverse)
library(brms)
library(loo)

# cargamos los datos
dataSP3 <- st_read("TFG/archivos/dataSP3.shp")

# construyo la variable antiguedad a partir de F_APERTURA
dataSP3 <- dataSP3  %>% mutate(ANTIGUEDAD = (as.Date("2022-12-23") - F_APERT)/365.25)

# transformacion de variables
dataSP3$CATEGOR <- as.factor(dataSP3$CATEGOR)

dataSP3$BIM6 <- as.numeric(dataSP3$BIM6) # quedan como 0's y 1's tal y como brm quieren que sean
#dataSP3$BIM6 <- as.factor(dataSP3$BIM6)
table(dataSP3$BIM6)

dataSP3$BIM5 <- as.factor(dataSP3$BIM5)

dataSP3$TS_CORT <- as.factor(dataSP3$TS_CORT)

dataSP3$CCZ <- as.factor(dataSP3$CCZ)

summary(dataSP3) # 251 NA's en CANT_BIM_IMPAGOS
# dejando de lado los NA , intentar resolver en IM porque esas cuentas tienen NA

# sacamos los NA's en CANT_BIM_IMPAGOS
#dataSP3 <- dataSP3[!is.na(dataSP3$CANT_BI),] 
# ya esta solucionado

# brm pide que la variable de respuesta Y sea nuemerica, entonces
#dataSP3$BIM6 <- ifelse(dataSP3$BIM6=='impago',0,1) 
# no aparece como impago ahora

# chequeos
#which(!(st_is_valid(dataSP3))) # son las observaciones [183861, 233463]
# solucionamos
#dataSP3[c(183861, 233463),] <- st_make_valid(dataSP3[c(183861, 233463),])


# Verificar si el sistema de coordenadas es geográfico
crs <- st_crs(dataSP3)
isTRUE(st_is_longlat(crs)) #FALSE porque son proyectadas, correcto?


# Residuos de devianza: funcion para calcularlos
res_dev <-function(mod,y){
  p <- fitted(mod)[,1] # primera columna de eso
  e <- y - p # residuo "comun"
  s <- sign(e)
  d <- s*sqrt(-2*(y*log(p)+(1-y)*log(1-p)))
  return(d)
}


# Calcular los centroides de los polígonos
centroides <- st_centroid(dataSP3$geometry)

# Obtener las coordenadas de los centroides
coordenadas <- st_coordinates(centroides)

# Calcular la matriz de vecinos basada en la distancia entre los centroides
W <- dnearneigh(coordenadas, d1 = 0, d2 = 100, longlat = FALSE) #36 minutos aprox en hacer

# se debe volver a hacer! solo la W

# por el momento dejamos de lado el modelo completo


# hacerlo para los CCZ 5 y 17 porque uno tiene poca morosidad y otro tiene mucha

# TODO CCZ01 sin actualizar, falta cambiar a stye=W 

# En principio probamos el modelo para CCZ01
ccz1 <- dataSP3 %>% filter(CCZ == '1')

# Calcular los centroides de los polígonos
centroides01 <- st_centroid(ccz1$geometry)

# Obtener las coordenadas de los centroides
coordenadas01 <- st_coordinates(centroides01)

# Calcular la matriz de vecinos basada en la distancia entre los centroides
#W1 <- dnearneigh(coordenadas01, d1 = 0, d2 = 500, longlat = FALSE) #5 seg en hacer
W1 <- dnearneigh(coordenadas01, d1 = 0, d2 = 100, longlat = FALSE)
W1

W250 <- dnearneigh(coordenadas01, d1 = 0, d2 = 250, longlat = FALSE)
W250 # 1 region sin links

W50 <- dnearneigh(coordenadas01, d1 = 0, d2 = 50, longlat = FALSE)
W50 # 14 regiones sin links

W25 <- dnearneigh(coordenadas01, d1 = 0, d2 = 25, longlat = FALSE)
W25 # 160 regiones sin links


nb_l <- list(d1 = W25, d2 = W50, d3 = W250)
sapply(nb_l, function(x) is.symmetric.nb(x, verbose = FALSE, force = TRUE)) #true all


sapply(nb_l, function(x) n.comp.nb(x)$nc)
# d1   d2   d3 
#541  31   2 

'
isTRUE(all.equal(attr(W25, "region.id"), row.names(as(ccz1,"data.frame")))) # no me sale
plot(ccz1$geometry, border = "grey60")
plot(W25, ccz1$geometry, pch = 19, cex = 0.6, add = TRUE)
'

W20 <- dnearneigh(coordenadas01, d1 = 0, d2 = 20, longlat = FALSE)

W15 <- dnearneigh(coordenadas01, d1 = 0, d2 = 15, longlat = FALSE)

W10 <- dnearneigh(coordenadas01, d1 = 0, d2 = 10, longlat = FALSE)
W10 # tiene 1622 regiones with no links

# Hacemos Moran Test para ver si hay autocorrelacion espacial en los residuos, para ello usa nb2listw
WlB1 <- nb2listw(W1 ,style = 'B', zero.policy = TRUE) 
WlW1 <- nb2listw(W1 ,style = 'W', zero.policy = TRUE) 
WlS1 <- nb2listw(W1 ,style = 'S', zero.policy = TRUE) # estandarizada

WlS250 <- nb2listw(W250 ,style = 'S', zero.policy = TRUE) # estandarizada para vecinos a 250m

WlS50 <- nb2listw(W50 ,style = 'S', zero.policy = TRUE) # estandarizada para vecinos a 50m

WlS25 <- nb2listw(W25 ,style = 'S', zero.policy = TRUE) # estandarizada para vecinos a 25m

WlS20 <- nb2listw(W20 ,style = 'S', zero.policy = TRUE) # estandarizada para vecinos a 20m

WlS15 <- nb2listw(W15 ,style = 'S', zero.policy = TRUE) # estandarizada para vecinos a 15m

WlS10 <- nb2listw(W10 ,style = 'S', zero.policy = TRUE) # estandarizada para vecinos a 10m

# Hacemos el modelo sin la variable estrella y con la variable estrella
ahora1.s <- Sys.time()
mod1.s <- brm(BIM6 ~ CATEGOR + PROP_BIM_IMPAGOS + CANT_UN + ANTIGUEDAD, data = ccz1, family = binomial() ) 
ahora1.s <- Sys.time() - ahora1.s # 6 mins 


ahora1.c <- Sys.time()
mod1.c <- brm(BIM6 ~ CATEGOR + BIM5 + PROP_BIM_IMPAGOS + CANT_UN + ANTIGUEDAD, data = ccz1, family = binomial() ) 
ahora1.c <- Sys.time() - ahora1.c # 6.4 mins aprox


# Obtenemos los resiudos: comunes y de devianza

# Residuos comunes: e = y - y^gorro
#mod1_res <- residuals(mod1) # le lleva unos segundos


# Residuos de devianza:
mod1.s_res.d <- res_dev(mod1.s,ccz1$BIM6)
mod1.c_res.d <- res_dev(mod1.c,ccz1$BIM6)


# es inutil dada la naturaleza de la variable de respuesta
moran.plot(mod1.s_res.d, listw = WlS1,
           xlab = "BIMESTRE 6 (Y)",
           ylab = "Rezago espacial del BIM6 (WY)",
           main = c("Diagrama de dispersion de Moran")) # FEO


# Si hacemos la prueba de hipotesis sobre los RESIDUOS del modelo
#moran.test(mod1_res[,1], WlW1.1) # estimacion del indice de moran = 1.103776e-03 es minuscula = MALO!
# el p-valor = 0.2367 no es tan bajo, pues no hay indicios de rechazar H0 es decir parece no haber autocorrelacion espacial LLORO!
#moran.test(mod1_res[,1], WlB1.1)

moran.test(mod1_res[,1], WlW1.1, randomisation=FALSE) # asumiendo normalidad, resultado casi identico

# con residuos de devianza
moran.test(mod1.s_res.d, WlW1, zero.policy = TRUE) 
moran.test(mod1.s_res.d, WlB1, zero.policy = TRUE) 
moran.test(mod1.s_res.d, WlS1, zero.policy = TRUE)

moran.test(mod1.c_res.d, WlW1, zero.policy = TRUE) 
moran.test(mod1.c_res.d, WlB1, zero.policy = TRUE) 
moran.test(mod1.c_res.d, WlS1, zero.policy = TRUE)


moran.test(mod1.s_res.d, WlS250, zero.policy = TRUE) # no es peor que el anterior p-valor = 0.02216 pero estimacion = 1.231745e-03
moran.test(mod1.c_res.d, WlS250, zero.policy = TRUE) # demora mas que para 100m y el resultado es peor, estimacion mas chica y p-valor mas grande

moran.test(mod1.s_res.d, WlS50, zero.policy = TRUE) # es el mejor resultado hasta ahora
moran.test(mod1.c_res.d, WlS50, zero.policy = TRUE) # 

moran.test(mod1.s_res.d, WlS25, zero.policy = TRUE) # aun mejor
moran.test(mod1.c_res.d, WlS25, zero.policy = TRUE) # 

moran.test(mod1.s_res.d, WlS20, zero.policy = TRUE) # empeora
moran.test(mod1.c_res.d, WlS20, zero.policy = TRUE) # 

moran.test(mod1.s_res.d, WlS15, zero.policy = TRUE) # y mejora, hasta ahora el mejor
moran.test(mod1.c_res.d, WlS15, zero.policy = TRUE) # empeora mas

moran.test(mod1.s_res.d, WlS10, zero.policy = TRUE) # no es malo
moran.test(mod1.c_res.d, WlS10, zero.policy = TRUE) # mejora



# brm necesitar usar nb2mat
WmB1 <- nb2mat(W1, style = "B", zero.policy = TRUE) # argumento zero.policy = TRUE permite regiones sin vecinos
WmW1 <- nb2mat(W1, style = "W", zero.policy = TRUE)


# repetir todo con la W que tiene la region sin vecinos, y jugar con las distancias que tenemos en W mayor y menor a 100
# en el modelo sin BIM5




# Repito todo pero para CCZ9 el cual creo puede tener una mayor autocorrelacion espacial dado la mayor cant de obs. y la posicion geografica
ccz9 <- dataSP3 %>% filter(CCZ == '9')

# Calcular los centroides de los polígonos
centroides09 <- st_centroid(ccz9$geometry)

# Obtener las coordenadas de los centroides
coordenadas09 <- st_coordinates(centroides09)

# sobreescribo los W ya creados
# Calcular la matriz de vecinos basada en la distancia entre los centroides
W1 <- dnearneigh(coordenadas09, d1 = 0, d2 = 100, longlat = FALSE) # le cuesta mas tiempo hacerlo (menos de 1 min)
W1 # 37 regiones without links

W250 <- dnearneigh(coordenadas09, d1 = 0, d2 = 250, longlat = FALSE)
W250 # 12 regiones sin links

W50 <- dnearneigh(coordenadas09, d1 = 0, d2 = 50, longlat = FALSE)
W50 # 151 regiones sin links

W25 <- dnearneigh(coordenadas09, d1 = 0, d2 = 25, longlat = FALSE)
W25 # 649 regiones sin links

W20 <- dnearneigh(coordenadas09, d1 = 0, d2 = 20, longlat = FALSE)
W20 # 1313 regiones sin links

W15 <- dnearneigh(coordenadas09, d1 = 0, d2 = 15, longlat = FALSE)
W15 # 3103 regiones sin links

W10 <- dnearneigh(coordenadas09, d1 = 0, d2 = 10, longlat = FALSE)
W10 # tiene 10470 regiones with no links aprox un 33% de las mismas, son muchas

W500 <- dnearneigh(coordenadas09, d1 = 0, d2 = 500, longlat = FALSE)
W500 # 6 regiones sin links


# Hacemos Moran Test para ver si hay autocorrelacion espacial en los residuos, para ello usa nb2listw
WlW1 <- nb2listw(W1 ,style = 'W', zero.policy = TRUE) # estandarizada

WlW250 <- nb2listw(W250 ,style = 'W', zero.policy = TRUE) # estandarizada para vecinos a 250m

WlW50 <- nb2listw(W50 ,style = 'W', zero.policy = TRUE) # estandarizada para vecinos a 50m

WlW25 <- nb2listw(W25 ,style = 'W', zero.policy = TRUE) # estandarizada para vecinos a 25m

WlW20 <- nb2listw(W20 ,style = 'W', zero.policy = TRUE) # estandarizada para vecinos a 20m

WlW15 <- nb2listw(W15 ,style = 'W', zero.policy = TRUE) # estandarizada para vecinos a 15m

WlW10 <- nb2listw(W10 ,style = 'W', zero.policy = TRUE) # estandarizada para vecinos a 10m

WlW500 <- nb2listw(W500 ,style = 'W', zero.policy = TRUE) # estandarizada para vecinos a 500m


# Hacemos el modelo sin la variable estrella y con la variable estrella
ahora9.s <- Sys.time()
mod9.s <- brm(BIM6 ~ CATEGOR + PROP_BIM_IMPAGOS + CANT_UN + ANTIGUEDAD, data = ccz9, family = binomial() ) 
ahora9.s <- Sys.time() - ahora9.s # 17.6 mins


ahora9.c <- Sys.time()
mod9.c <- brm(BIM6 ~ CATEGOR + BIM5 + PROP_BIM_IMPAGOS + CANT_UN + ANTIGUEDAD, data = ccz9, family = binomial() ) 
ahora9.c <- Sys.time() - ahora9.c #  15.2 mins


# Obtenemos los resiudos: comunes y de devianza

# Residuos comunes: e = y - y^gorro
#mod1_res <- residuals(mod1) # le lleva unos segundos

# Residuos de devianza:
mod9.s_res.d <- res_dev(mod9.s,ccz9$BIM6)
mod9.c_res.d <- res_dev(mod9.c,ccz9$BIM6)


# Si hacemos la prueba de hipotesis de moran test

# sobre los RESIDUOS comunes del modelo
#moran.test(mod1_res[,1], WlW1.1) # estimacion del indice de moran = 1.103776e-03 es minuscula = MALO!
# el p-valor = 0.2367 no es tan bajo, pues no hay indicios de rechazar H0 es decir parece no haber autocorrelacion espacial LLORO!
#moran.test(mod1_res[,1], WlB1.1)

# con residuos de devianza
mt.9sW1 <- moran.test(mod9.s_res.d, WlW1, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 7.309432e-02 con W=6.289327e-02
mt.9cW1 <- moran.test(mod9.c_res.d, WlW1, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 8.437490e-02 con W=7.676916e-02

mt.9sW250 <- moran.test(mod9.s_res.d, WlW250, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 3.562702e-02 con W=3.774517e-02
mt.9cW250 <- moran.test(mod9.c_res.d, WlW250, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 4.403601e-02 con W=4.696750e-02

mt.9sW50 <- moran.test(mod9.s_res.d, WlW50, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.069565e-01 con W=8.274026e-02
mt.9cW50 <- moran.test(mod9.c_res.d, WlW50, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.197076e-01 con W=9.921231e-02

mt.9sW25 <- moran.test(mod9.s_res.d, WlW25, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.296133e-01 con W=1.006796e-01
mt.9cW25 <- moran.test(mod9.c_res.d, WlW25, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.411127e-01 con W=1.206807e-01

mt.9sW20 <- moran.test(mod9.s_res.d, WlW20, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.329787e-01 con W=1.050023e-01
mt.9cW20 <- moran.test(mod9.c_res.d, WlW20, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.427500e-01 con W=1.229162e-01

mt.9sW15 <- moran.test(mod9.s_res.d, WlW15, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.310347e-01 con W=1.084727e-01
mt.9cW15 <- moran.test(mod9.c_res.d, WlW15, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.400116e-01 con W=1.246725e-01

mt.9sW10 <- moran.test(mod9.s_res.d, WlW10, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.051660e-01 con W=9.885986e-02
mt.9cW10 <- moran.test(mod9.c_res.d, WlW10, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.086057e-01 con W=1.025054e-01

mt.9sW500 <- moran.test(mod9.s_res.d, WlW500, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 1.576344e-02 con W=1.810588e-02
mt.9cW500 <- moran.test(mod9.c_res.d, WlW500, zero.policy = TRUE) # p-valor = 2.2e-16 e indice de Moran = 2.095755e-02 con W=2.423127e-02
# demoran en hacer



# Integramos al modelo el componente espacial car(), modelo que incluye variable explicativa BIM5 para el CCZ09:

# brm() necesitar usar nb2mat
# 
centroides09 <- st_centroid(ccz9$geometry)
coordenadas09 <- st_coordinates(centroides09)
W50 <- dnearneigh(coordenadas09, d1 = 0, d2 = 50, longlat = FALSE)
W50

WmB9.50 <- nb2mat(W50, style = "B", zero.policy = TRUE) # para car() la style='B'

# Corremos modelo demora
ahora9car <- Sys.time()
mod9car <- brm(BIM6 ~ CATEGOR + BIM5 + PROP_BIM_IMPAGOS + CANT_UN + ANTIGUEDAD + car(WmB9.50, gr = NA, type = 'escar'),
               data = ccz9, data2 = list(WmB9.50=WmB9.50), family = binomial() )  
ahora9car <- Sys.time() - ahora9car
# ahora9car = 

'Using the maximum response value as the number of trials.
Only 2 levels detected so that family 'bernoulli' might be a more efficient choice.
Error: For exact sparse CAR, all locations should have at least one neighbor within the provided data set. Consider using type = 'icar' instead.
In addition: Warning messages:
1: Using 'binomial' families without specifying 'trials' on the left-hand side of the model formula is deprecated. 
2: Using CAR terms without a grouping factor is deprecated. Please use argument 'gr' even if each observation represents its own location. '

'No funciono al correr el modelo mod9car sin eliminar regiones sin vecinos salta'

# Rdata de mod9car se guardo antes de correr lo que sigue. por lo tanto se debe correr ccz9.1 en adelante
ccz9.1 <- ccz9[-c(4,90,92,117,138,139,140,141,144,218,263,266,303,306,334,381,1146,1325,1342,1345,1357,1394,1396,1428,1860,1941,2462,2596,2657,
                  2732,3019,3037,3973,4087,4100,4171,4177,4645,5276,5298,5318,5329,6467,6827,9413,9416,10044,10473,10493,10655,10815,11227,
                  11451,11463,11869,11964,12165,12179,12195,12246,12306,12625,12683,12799,13690,13828,14414,14888,14910,14981,15171,15305,15436,
                  15479,16146,16262,16373,16401,16816,17055,17202,17258,17466,17599,17624,19396,19976,20279,20928,21083,21145,21237,21256,21846,
                  22280,22326,22337,22526,22547,22560,22564,22577,22598,22834,22943,23004,23336,23572,23644,23855,23932,24649,24877,25004,25188,
                  25299,25661,25718,25719,25738,25758,25961,25983,25992,25998,26117,26227,26292,26473,26657,26810,26829,27021,27173,27350,28476,
                  29095,29121,29138,29227,29232,29296,29415,29471,30080,30194,30196,30197,30199,30200,30266),]
centroides09.1 <- st_centroid(ccz9.1$geometry)
coordenadas09.1 <- st_coordinates(centroides09.1)
W50 <- dnearneigh(coordenadas09.1, d1 = 0, d2 = 50, longlat = FALSE) # sin regiones con 0 vecinos
W50 # si porcentaje of nonzero weights fuese 100% es porque todos son vecinos de todos

#WmW9.50 <- nb2mat(W50, style = "W") # no es simetrica
WmB9.50 <- nb2mat(W50, style = "B") # para car() la style='B' 

# Corremos modelo demora, probablemente el alpha sea cercano a 0
ahora9car <- Sys.time()
mod9car <- brm(BIM6 ~ CATEGOR + BIM5 + PROP_BIM_IMPAGOS + CANT_UN + ANTIGUEDAD + car(WmB9.50, gr = NA, type = 'escar'),
              data = ccz9.1, data2 = list(WmB9.50=WmB9.50), family = binomial() )  
ahora9car <- Sys.time() - ahora9car
# ahora9car = 

'salta el siguiente error a los pocos minutos de estar corriendo:
Error in t(inv_sqrt_D) %*% M : 
  Cholmod error 'out of memory' at file ../Core/cholmod_memory.c, line 146'

summary(mod9car)

plot(mod9car)

'{ pasos para borrar todo el environment en expecion de algunos objetos
# Lista de objetos que deseas conservar
objetos_a_conservar <- c("ccz9", "WmW9.50")

# Obtén todos los objetos en el espacio de trabajo
objetos_en_espacio_trabajo <- ls()

# Elimina todos los objetos, excepto los que deseas conservar
objetos_a_eliminar <- setdiff(objetos_en_espacio_trabajo, objetos_a_conservar)
rm(list = objetos_a_eliminar)
}'



# Es necesario excluir la region sin enlaces en W1? APARENTEMENTE SI
ccz1.1 <- ccz1[-1,]

centroides01.1 <- st_centroid(ccz1.1$geometry)
coordenadas01.1 <- st_coordinates(centroides01.1)

W1_1.1 <- dnearneigh(coordenadas01.1, d1 = 0, d2 = 100, longlat = FALSE) # sin regiones con 0 vecinos
W1_1.1 # si porcentaje of nonzero weights fuese 100% es porque todos son vecinos de todos


# Hacemos Moran Test para ver si hay autocorrelacion espacial en los residuos, para ello usa nb2listw
# Matriz de ady sin regiones sin vecinos
WlW1.1 <- nb2listw(W1_1.1, style = 'W')

# Hacemos el modelo sin la region 1 (la cual no tiene vecinos)
ahora1 <- Sys.time()
mod1 <- brm(BIM6 ~ CATEGOR + BIM5 + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz1.1, family = binomial() ) 
ahora1 <- Sys.time() - ahora1 # 8 mins

summary(mod1) # utilizo 8344 observaciones

# calculamos WAIC (es como el AIC)
m1 <- waic(mod1)

# Obtenemos los resiudos: comunes y de devianza

# Residuos comunes: e = y - y^gorro
mod1_res <- residuals(mod1) # le lleva unos segundos

# Residuos de devianza: 
mod1_res.d <- res_dev(mod1,ccz1.1$BIM6)



# es inutil dada la naturaleza de la variable de respuesta
moran.plot(mod1_res[,1], listw = WlW1.1,
           xlab = "BIMESTRE 6 (Y)",
           ylab = "Rezago espacial del BIM6 (WY)",
           main = c("Diagrama de dispersion de Moran")) # FEO



# Si hacemos la prueba de hipotesis sobre los RESIDUOS del modelo
moran.test(mod1_res[,1], WlW1.1) # estimacion del indice de moran = 1.103776e-03 es minuscula = MALO!
# el p-valor = 0.2367 no es tan bajo, pues no hay indicios de rechazar H0 es decir parece no haber autocorrelacion espacial LLORO!
moran.test(mod1_res[,1], WlB1.1)


moran.test(mod1_res[,1], WlW1.1, randomisation=FALSE) # asumiendo normalidad, resultado casi identico

# correr WlW1.1 <- nb2listw
# con residuos de devianza
moran.test(mod1_res.d, WlW1.1) 





# Integramos al modelo el componente espacial car(), modelo que incluye variable explicativa BIM5 para el CCZ1:

# brm() necesitar usar nb2mat
# W1_1.1 es la matriz de ady sin la region sin vecinos
WmB1.1 <- nb2mat(W1_1.1, style = "B") 
WmW1.1 <- nb2mat(W1_1.1, style = "W") 


# Corremos modelo demora, probablemente el alpha sea cercano a 0
ahora1.t <- Sys.time()
mod1.t <- brm(BIM6 ~ BIM5 + CATEGOR + CANT_BI + CANT_UN + ANTIGUEDAD + car(WmB1.1, gr = NA, type = 'escar'),
              data = ccz1.1, data2 = list(WmB1.1=WmB1.1), family = binomial() )  
ahora1.t <- Sys.time() - ahora1.t
# ahora1.t = 5.8 hours

summary(mod1.t)

plot(mod1.t)

# calculamos WAIC
m2 <- waic(mod1.t)

# el WAIC menor es mejor
loo_compare(m1, m2, criterion = "waic")



