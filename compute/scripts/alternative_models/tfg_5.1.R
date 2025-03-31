# CAR (spatial conditional autoreggresive)

# construir facet con los 4 coeficeientes de los OR
# y otra fila con los del modelo completo


library(spdep)
library(tidyverse)
library(ggplot2)
library(brms)
library(bayesplot) # for traceplots visualizations

library(loo)

# cargamos los datos
dataSP3 <- st_read("TFG/archivos/dataSP3.shp")

# construyo la variable antiguedad a partir de F_APERTURA
dataSP3 <- dataSP3  %>% mutate(ANTIGUEDAD = (as.Date("2022-12-23") - F_APERT)/365.25)

# transformacion de variables
dataSP3$CATEGOR <- as.factor(dataSP3$CATEGOR)

dataSP3$BIM6 <- as.numeric(dataSP3$BIM6) # quedan como 0's y 1's numeric tal y como brm quieren que sean

dataSP3$BIM5 <- as.factor(dataSP3$BIM5)

dataSP3$TS_CORT <- as.factor(dataSP3$TS_CORT)

dataSP3$CCZ <- as.factor(dataSP3$CCZ)

summary(dataSP3)

# chequeos
#which(!(st_is_valid(dataSP3))) # none

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

#------------------------------------------------------------------------------#
# se contstruyen las matrices de vecinos y de pesos espaciales

ccz17 <- dataSP3 %>% filter(CCZ == '17')

# Calcular los centroides de los polígonos
centroides17 <- st_centroid(ccz17$geometry)

# Obtener las coordenadas de los centroides
coordenadas17 <- st_coordinates(centroides17)

# Calcular la matriz de vecinos basada en la distancia entre los centroides
W500.17 <- dnearneigh(coordenadas17, d1 = 0, d2 = 500, longlat = FALSE)
W500.17 # 0 regiones sin links

W250.17 <- dnearneigh(coordenadas17, d1 = 0, d2 = 250, longlat = FALSE)
W250.17 # 1 regiones sin links

W100.17 <- dnearneigh(coordenadas17, d1 = 0, d2 = 100, longlat = FALSE) 
W100.17 # 10 regiones without links

W50.17 <- dnearneigh(coordenadas17, d1 = 0, d2 = 50, longlat = FALSE)
W50.17 # 40 regiones sin links

W30.17 <- dnearneigh(coordenadas17, d1 = 0, d2 = 30, longlat = FALSE)
W30.17 # 159 regiones sin links

W15.17 <- dnearneigh(coordenadas17, d1 = 0, d2 = 15, longlat = FALSE)
W15.17 # 970 regiones sin links

#---- visualizations ----
# se crean tables para distinuir la dsitribucion cantidad de vecinos prr matriz
enlaces1.17 <- card(W500.17)
enlaces2.17 <- card(W250.17)
enlaces3.17 <- card(W100.17)
enlaces4.17 <- card(W50.17)
enlaces5.17 <- card(W30.17)
enlaces6.17 <- card(W15.17)

tabla_enlaces1.17 <- table(enlaces1.17)
tabla_enlaces2.17 <- table(enlaces2.17)
tabla_enlaces3.17 <- table(enlaces3.17)
tabla_enlaces4.17 <- table(enlaces4.17)
tabla_enlaces5.17 <- table(enlaces5.17)
tabla_enlaces6.17 <- table(enlaces6.17)

summary(enlaces1.17)
summary(enlaces6.17)

# summary de las distancias por matriz
dsts0 <- unlist(nbdists(W500.17, st_centroid(ccz17$geometry)))
summary(dsts0)

# visualizacion de la distribucion de vecinos por distancias
# lista con todas las tablas
lista_tablas_enlaces <- list(tabla_enlaces1.17, tabla_enlaces2.17, tabla_enlaces3.17, tabla_enlaces4.17, tabla_enlaces5.17, tabla_enlaces6.17)

# se convierten en dataframe
lista_dataframes <- lapply(lista_tablas_enlaces, as.data.frame)

# se definen los intervalos que quiero para observar la distribucion de vecinos por distancias
intervalos <- c(0, 1, 2, 3, 4, 5, 10, 20, 50, 100, 200, Inf)

# matriz vacia df_intervalo para iterar sobre ella
num_filas <- length(intervalos) - 1
num_columnas <- length(lista_dataframes)
df_intervalos <- matrix(0, nrow = num_filas, ncol = num_columnas)

# se caluculan las cantidades por intervalo
for (i in 1:num_columnas) {
  df_actual <- lista_dataframes[[i]]
  df_actual[,1] <- as.character(df_actual[,1]) # as.character para que mantnega el formato numerico de los factores
  df_actual[,1] <- as.numeric(df_actual[,1])
  
    for (j in 1:num_filas) {
    min_intervalo <- intervalos[j]
    max_intervalo <- intervalos[j + 1]
    
    # calcular la suma de valores en la columna 2 cuando se cumple la condición en la columna 1
    suma_valores <- sum(df_actual[df_actual[, 1] >= min_intervalo & df_actual[, 1] < max_intervalo, 2])
    
    # asignar el resultado en df_intervalos
    df_intervalos[j, i] <- suma_valores
  }
}

# convertir df_intervalos en un dataframe con nombres de columna
df_intervalos <- as.data.frame(df_intervalos)
colnames(df_intervalos) <- c('500m','250m','100m',
                             '50m','30m','15m')
rownames(df_intervalos) <- c('0', '1', '2', '3', '4', '[5,10)', '[10,20)', 
                             '[20,50)', '[50,100)', '[100,200)', '[200,+Inf)')

# sumamos la columna con los intervalos
df_intervalos2 <- cbind(df_intervalos, Intervalos=c('0', '1', '2', '3', '4', '[5,10)', '[10,20)', 
                        '[20,50)', '[50,100)', '[100,200)', '[200,+Inf)'))

# se define el orden de los intervalos
df_intervalos2$Intervalos <- factor(rownames(df_intervalos), levels = orden_intervalos)

# se convierte el dataframe de formato ancho a largo (tidy data)
df_intervalos_long <- pivot_longer(df_intervalos2, 
                                   cols = -Intervalos, 
                                   names_to = "Distancias", 
                                   values_to = "Frecuencia")

# se define el orden deseado en distancias
orden_distancias <- c(
  '15m', '30m', '50m',
  '100m', '250m', '500m')
df_intervalos_long$Distancias <- factor(df_intervalos_long$Distancias, levels = orden_distancias)

# se crea rafico de barras de la distribucion de vecinos por intervalos dados seun distancia definida
ggplot(df_intervalos_long, aes(x = Intervalos, y = Frecuencia, fill = Distancias)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "",
    x = "Cantidad de Vecinos",
    y = "Frecuencia"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-----------------------------------------------------------------------------#
#---- moran tests ----
# contstruimos el modelo
#ccz17$BIM6 <- ifelse(ccz17$BIM6=='0',0,1)
#table(ccz17$BIM6)

# Hacemos el modelo sin la variable estrella y con la variable estrella
ahora17.s <- Sys.time()
mod17.s <- brm(BIM6|trials(1) ~ CATEGOR + CANT_UN + PROP_BI + ANTIGUEDAD, data = ccz17, family = binomial())
ahora17.s <- Sys.time() - ahora17.s # 7 mins

summary(mod17.s, waic = TRUE)

# distribuciones posteriores y convergencias
plot(mod17.s)

# traceplots
mcmc_trace(mod17.s)

# waic
waic17.s <- loo(mod17.s, cores = 4) # demora en hacer
'Computed from 4000 by 9737 log-likelihood matrix

         Estimate    SE
elpd_loo  -4435.8  62.6
p_loo         6.9   0.4
looic      8871.6 125.2
------
Monte Carlo SE of elpd_loo is 0.0.

All Pareto k estimates are good (k < 0.5).
See help('pareto-k-diagnostic') for details.'

# modelo con BIM5
ahora17.c <- Sys.time()
mod17.c <- brm(BIM6|trials(1) ~ CATEGOR + CANT_UN + BIM5 + PROP_BI + ANTIGUEDAD, data = ccz17, family = binomial() ) 
ahora17.c <- Sys.time() - ahora17.c #  11.2 mins

summary(mod17.c, waic = TRUE)

odds <- exp(summary(mod17.c, waic = TRUE)$fixed[,'Estimate'])  # Exponencial de los coeficientes
probabilidades <- odds / (1 + odds) 
# si la variable categoria es domiciliaria corresponde a un aumento del 51% en la prob. de pago del BIM6
# si la cant_un

# distribuciones posteriores y convergencias
plot(mod17.c)

# traceplots
mcmc_trace(mod17.c)

# waic
waic17.c <- loo(mod17.c, cores = 4) # demora en hacer



# Obtenemos los resiudos: comunes y de devianza

# Residuos comunes: e = y - y^gorro
#mod1_res <- residuals(mod1) # le lleva unos segundos

# Residuos de devianza:
mod17.s_res.d <- res_dev(mod17.s,ccz17$BIM6)
mod17.c_res.d <- res_dev(mod17.c,ccz17$BIM6)


# hacemos Moran Test para ver si hay autocorrelacion espacial en los residuos, para ello usa nb2listw
# para ello asignamos los objetos nb creados en una lista
W_nb.17 <- list(W15.17, W30.17, W50.17, W100.17, W250.17, W500.17)

listw.17 <- lapply(W_nb.17, function(x){nb2listw(x, style = 'W', zero.policy = TRUE)})


# Si hacemos la prueba de hipotesis de moran test
# con residuos de devianza

# con residuos de mod17.s_res.d
moran_tests.s.17 <- lapply(listw.17, function(listw){moran.test(mod17.s_res.d, listw, zero.policy = TRUE)}) # demora

#lm.morantest(nyGLMp, listw = NYlistwW)

# con residuos de mod17.c_res.d
moran_tests.c.17 <- lapply(listw.17, function(listw){moran.test(mod17.c_res.d, listw, zero.policy = TRUE)}) # demora

'
Realizamos el test utilizando simulación Monte Carlo con la función *moran.mc()*:
  
  ```{r,warning=FALSE}
mmc1<-moran.mc(Uruguay$YSVL,listw1,nsim=99)
mmc1
```

Hacemos un histograma de los datos simulados:
  ```{r,warning=FALSE}
hist(mmc1$res, main="Distribución empírica del Índice de Moran")


Un gráfico muy utilizado es el que se conoce como "Moran Plot". En él se grafica en el eje de coordenadas a la variable $Y$, y en el de ordenadas su valor rezagado, $WY$.

```{r,warning=FALSE}
lag.y=moran.plot(Uruguay$YSVL,listw1,return_df = TRUE)
'

# me quede por aca

# seuir con esto

mmc1 <- moran.mc(mod17.s_res.d, listw.17[[1]], nsim = 999, zero.policy = TRUE)
mmc1

hist(mmc1$res, main="Distribución empírica del Índice de Moran")




moran.plot(mod17.s_res.d, listw.17[[3]], return_df = TRUE)
moran.plot(mod17.c_res.d, listw.17[[3]], return_df = TRUE)
# se visualizan unas tendencias que no deberiamos ver, lo cual
# reflejan existencia de autocorrelacion espacial.







#---- car models ----
# Integramos al modelo el componente espacial car(), modelo que incluye variable explicativa BIM5 para el CCZ09:

# brm() necesitar usar nb2mat
# 
centroides17 <- st_centroid(ccz17$geometry)
coordenadas17 <- st_coordinates(centroides17)

#W50.17 # weights neieghbours list

Wm50.17 <- nb2mat(W50.17, style = "B", zero.policy = TRUE) # para car() la style='B'

# car
ahora17car <- Sys.time()
mod17.c.car <- brm(BIM6|trials(1) ~ CATEGOR + CANT_UN + BIM5 + PROP_BI + ANTIGUEDAD + car(Wm50.17, gr = NA, type = 'escar'), 
                    data = ccz17, data2 = list(Wm50.17 = Wm50.17), family = binomial() )  
ahora17car <- Sys.time() - ahora17car
# 

'Only 2 levels detected so that family 'bernoulli' might be a more efficient choice.
Error: For exact sparse CAR, all locations should have at least one neighbor within the provided data set. Consider using type = 'icar' instead.
Además: Warning message:
Using CAR terms without a grouping factor is deprecated. Please use argument 'gr' even if each observation represents its own location.'

# icar: si se corre otra vez se debe cambiar el nombre a mod17.c.icar
ahora17car <- Sys.time()
mod17.c.car <- brm(BIM6|trials(1) ~ CATEGOR + CANT_UN + BIM5 + PROP_BI + ANTIGUEDAD + car(Wm50.17, gr = NA, type = 'icar'), 
                   data = ccz17, data2 = list(Wm50.17 = Wm50.17), family = binomial() )  
ahora17car <- Sys.time() - ahora17car

"Warning messages:
1: Using CAR terms without a grouping factor is deprecated. Please use argument 'gr' even if each observation represents its own location. 
2: There were 4000 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
3: Examine the pairs() plot to diagnose sampling problems
 
4: The largest R-hat is 3.27, indicating chains have not mixed.
Running the chains for more iterations may help. See
https://mc-stan.org/misc/warnings.html#r-hat 
5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
Running the chains for more iterations may help. See
https://mc-stan.org/misc/warnings.html#bulk-ess 
6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
Running the chains for more iterations may help. See
https://mc-stan.org/misc/warnings.html#tail-ess "

summary(mod17.c.car)

plot(mod17.c.car)

waic17.c.car <- loo(mod17.c.car, cores = 4) # demora en hacer

comparando <- waic(mod17.c.car)
waic17.c.car # con BIM5 y con ICAR: elpd_loo  -2368.6
waic17.c # con BIM5: elpd_loo  -2369.0
waic17.s # sin BIM5: elpd_loo  -4435.8

# icar cte
ahora17icar <- Sys.time()
mod17.cte <- brm(BIM6|trials(1) ~ 1 + car(Wm50.17, gr = NA, type = 'icar'), 
                   data = ccz17, data2 = list(Wm50.17 = Wm50.17), family = binomial() )  
ahora17cte <- Sys.time() - ahora17cte

summary(mod17.cte)
plot(mod17.cte)


# ICAR sin BIM5 , falta acer
ahora17.s.icar <- Sys.time()
mod17.s.icar <- brm(BIM6|trials(1) ~ CATEGOR + CANT_UN + PROP_BI + ANTIGUEDAD + car(Wm50.17, gr = NA, type = 'icar'), 
                   data = ccz17, data2 = list(Wm50.17 = Wm50.17), family = binomial(), 
                   iter = 3000, chains = 3)  
ahora17.s.icar <- Sys.time() - ahora17.s.icar









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















#-------------------------------------------------------------------------------#
# hacerlo para los CCZ 5 y 17 porque uno tiene poca morosidad y otro tiene mucha

table(dataSP3$CCZ)

ccz5 <- dataSP3 %>% filter(CCZ == '5')

# Calcular los centroides de los polígonos
centroides5 <- st_centroid(ccz5$geometry)

# Obtener las coordenadas de los centroides
coordenadas5 <- st_coordinates(centroides5)

# Calcular la matriz de vecinos basada en la distancia entre los centroides
W500.5 <- dnearneigh(coordenadas5, d1 = 0, d2 = 500, longlat = FALSE)
W500.5 # 1 regiones sin links

W250.5 <- dnearneigh(coordenadas5, d1 = 0, d2 = 250, longlat = FALSE)
W250.5 # 1 regiones sin links

W100.5 <- dnearneigh(coordenadas5, d1 = 0, d2 = 100, longlat = FALSE) 
W100.5 # 2 regiones without links

W50.5 <- dnearneigh(coordenadas5, d1 = 0, d2 = 50, longlat = FALSE)
W50.5 # 16 regiones sin links

W30.5 <- dnearneigh(coordenadas5, d1 = 0, d2 = 30, longlat = FALSE)
W30.5 # 58 regiones sin links

W15.5 <- dnearneigh(coordenadas5, d1 = 0, d2 = 15, longlat = FALSE)
W15.5 # 796 regiones sin links

# se crean tables para distinuir la dsitribucion cantidad de vecinos prr matriz
enlaces1.5 <- card(W500.5)
enlaces2.5 <- card(W250.5)
enlaces3.5 <- card(W100.5)
enlaces4.5 <- card(W50.5)
enlaces5.5 <- card(W30.5)
enlaces6.5 <- card(W15.5)

tabla_enlaces1.5 <- table(enlaces1.5)
tabla_enlaces2.5 <- table(enlaces2.5)
tabla_enlaces3.5 <- table(enlaces3.5)
tabla_enlaces4.5 <- table(enlaces4.5)
tabla_enlaces5.5 <- table(enlaces5.5)
tabla_enlaces6.5 <- table(enlaces6.5)

summary(enlaces1.5)
summary(enlaces4.5)
summary(enlaces6.5)

# summary de las distancias por matriz
dsts0 <- unlist(nbdists(W500.5, st_centroid(ccz5$geometry)))
summary(dsts0)

# visualizacion de la distribucion de vecinos por distancias
# lista con todas las tablas
lista_tablas_enlaces <- list(tabla_enlaces1.5, tabla_enlaces2.5, tabla_enlaces3.5, tabla_enlaces4.5, tabla_enlaces5.5, tabla_enlaces6.5)

# se convierten en dataframe
lista_dataframes <- lapply(lista_tablas_enlaces, as.data.frame)

# se definen los intervalos que quiero para observar la distribucion de vecinos por distancias
intervalos <- c(0, 1, 2, 3, 4, 5, 10, 20, 50, 100, 200, Inf)

# matriz vacia df_intervalo para iterar sobre ella
num_filas <- length(intervalos) - 1
num_columnas <- length(lista_dataframes)
df_intervalos <- matrix(0, nrow = num_filas, ncol = num_columnas)

# se caluculan las cantidades por intervalo
for (i in 1:num_columnas) {
  df_actual <- lista_dataframes[[i]]
  df_actual[,1] <- as.character(df_actual[,1]) # as.character para que mantnega el formato numerico de los factores
  df_actual[,1] <- as.numeric(df_actual[,1])
  
  for (j in 1:num_filas) {
    min_intervalo <- intervalos[j]
    max_intervalo <- intervalos[j + 1]
    
    # calcular la suma de valores en la columna 2 cuando se cumple la condición en la columna 1
    suma_valores <- sum(df_actual[df_actual[, 1] >= min_intervalo & df_actual[, 1] < max_intervalo, 2])
    
    # asignar el resultado en df_intervalos
    df_intervalos[j, i] <- suma_valores
  }
}

# convertir df_intervalos en un dataframe con nombres de columna
df_intervalos <- as.data.frame(df_intervalos)
colnames(df_intervalos) <- c('500m','250m','100m',
                             '50m','30m','15m')
rownames(df_intervalos) <- c('0', '1', '2', '3', '4', '[5,10)', '[10,20)', 
                             '[20,50)', '[50,100)', '[100,200)', '[200,+Inf)')

# sumamos la columna con los intervalos
df_intervalos2 <- cbind(df_intervalos, Intervalos=c('0', '1', '2', '3', '4', '[5,10)', '[10,20)', 
                                                    '[20,50)', '[50,100)', '[100,200)', '[200,+Inf)'))

# se define el orden de los intervalos
df_intervalos2$Intervalos <- factor(rownames(df_intervalos), levels = orden_intervalos)

# se convierte el dataframe de formato ancho a largo (tidy data)
df_intervalos_long <- pivot_longer(df_intervalos2, 
                                   cols = -Intervalos, 
                                   names_to = "Distancias", 
                                   values_to = "Frecuencia")

# se define el orden deseado en distancias
orden_distancias <- c(
  '15m', '30m', '50m',
  '100m', '250m', '500m')
df_intervalos_long$Distancias <- factor(df_intervalos_long$Distancias, levels = orden_distancias)

# se crea rafico de barras de la distribucion de vecinos por intervalos dados seun distancia definida
ggplot(df_intervalos_long, aes(x = Intervalos, y = Frecuencia, fill = Distancias)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "",
    x = "Cantidad de Vecinos",
    y = "Frecuencia"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-----------------------------------------------------------------------------#

# contstruimos el modelo
#ccz5$BIM6 <- ifelse(ccz5$BIM6=='0',0,1)
#table(ccz5$BIM6)

# Hacemos el modelo sin la variable estrella y con la variable estrella
ahora5.s <- Sys.time()
mod5.s <- brm(BIM6|trials(1) ~ CATEGOR + CANT_UN + PROP_BI + ANTIGUEDAD, data = ccz5, family = binomial())
ahora5.s <- Sys.time() - ahora5.s # 7 mins

summary(mod5.s, waic = TRUE)

# distribuciones posteriores y convergencias
plot(mod5.s)

# traceplots
mcmc_trace(mod5.s)

# waic
waic5.s <- loo(mod5.s, cores = 4) # demora en hacer
'Estimate    SE
elpd_loo  -6707.7  99.4
p_loo        11.6   0.9
looic     13415.4 198.8
------
Monte Carlo SE of elpd_loo is NA.

Pareto k diagnostic values:
                         Count Pct.    Min. n_eff
(-Inf, 0.5]   (good)     20544 100.0%  2682      
 (0.5, 0.7]   (ok)           1   0.0%  2968      
   (0.7, 1]   (bad)          2   0.0%  2995      
   (1, Inf)   (very bad)     0   0.0%  <NA>      
See help('pareto-k-diagnostic') for details'

# modelo con BIM5
ahora5.c <- Sys.time()
mod5.c <- brm(BIM6|trials(1) ~ CATEGOR + CANT_UN + BIM5 + PROP_BI + ANTIGUEDAD, data = ccz5, family = binomial() ) 
ahora5.c <- Sys.time() - ahora5.c #  11.2 mins

summary(mod5.c, waic = TRUE)

odds <- exp(summary(mod5.c, waic = TRUE)$fixed[,'Estimate'])  # Exponencial de los coeficientes
probabilidades <- odds / (1 + odds) 
# si la variable categoria es domiciliaria corresponde a un aumento del 51% en la prob. de pago del BIM6
# si la cant_un

# distribuciones posteriores y convergencias
plot(mod5.c)

# traceplots
mcmc_trace(mod5.c)

# waic
waic5.c <- loo(mod5.c, cores = 4) # demora en hacer
'
         Estimate    SE
elpd_loo  -4233.5  90.4
p_loo         7.2   0.5
looic      8466.9 180.7
------
Monte Carlo SE of elpd_loo is NA.

Pareto k diagnostic values:
                         Count Pct.    Min. n_eff
(-Inf, 0.5]   (good)     20541 100.0%  1659      
 (0.5, 0.7]   (ok)           3   0.0%  2509      
   (0.7, 1]   (bad)          2   0.0%  2450      
   (1, Inf)   (very bad)     1   0.0%  2391      
See help('pareto-k-diagnostic') for details.'

waic5.c <- waic(mod5.c)
'Computed from 4000 by 20547 log-likelihood matrix

          Estimate    SE
elpd_waic  -4233.5  90.4
p_waic         7.2   0.5
waic        8466.9 180.7'

# Obtenemos los resiudos: comunes y de devianza

# Residuos comunes: e = y - y^gorro
#mod1_res <- residuals(mod1) # le lleva unos segundos

# Residuos de devianza:
mod5.s_res.d <- res_dev(mod5.s,ccz5$BIM6)
mod5.c_res.d <- res_dev(mod5.c,ccz5$BIM6)


# hacemos Moran Test para ver si hay autocorrelacion espacial en los residuos, para ello usa nb2listw
# para ello asignamos los objetos nb creados en una lista
W_nb.5 <- list(W15.5, W30.5, W50.5, W100.5, W250.5, W500.5)

listw.5 <- lapply(W_nb.5, function(x){nb2listw(x, style = 'W', zero.policy = TRUE)})

'continuar desde aca, ya caragdo todo lo anterior faltan hacer los moran'

# Si hacemos la prueba de hipotesis de moran test
# con residuos de devianza

# con residuos de mod5.s_res.d
moran_tests.s.5 <- lapply(listw.5, function(listw){moran.test(mod5.s_res.d, listw, zero.policy = TRUE)}) # demora

#lm.morantest(nyGLMp, listw = NYlistwW)

# con residuos de mod5.c_res.d
moran_tests.c.5 <- lapply(listw.5, function(listw){moran.test(mod5.c_res.d, listw, zero.policy = TRUE)}) # demora

# grafico de Y , WY
moran.plot(mod5.s_res.d, listw.5[[3]], return_df = TRUE)
moran.plot(mod5.c_res.d, listw.5[[3]], return_df = TRUE)
