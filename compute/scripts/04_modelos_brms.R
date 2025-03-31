#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ---- LIBRARIES ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lapply(c("sf", "tidyverse", "brms", "ggplot2", "writexl", "caret", "spdep", 
         "bayesplot", "tmap", "xtable"), library, character.only = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ---- LOAD DATA ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# lectura de TDS totales
dataSP3 <- st_read("compute/data/dataSP3.shp") #1 min en cargar

# variable antiguedad
dataSP3 <- dataSP3 %>% 
  mutate(ANTIGUEDAD = as.period(interval(F_APERT, as.Date("2022-12-23")))/years(1))

# transformacion de variables
dataSP3$CATEGOR <- as.factor(dataSP3$CATEGOR)
dataSP3$CCZ <- as.factor(dataSP3$CCZ)
dataSP3$BIM6 <- as.factor(dataSP3$BIM6) 

# data
sin_generar <- dataSP3 %>% filter(BIM5 == -1)

# funcion para obtener residuos de devianza
dres <- function(mod, obs, brms = TRUE) {
  if (brms == TRUE) {
    e <- residuals(mod)[, 1]
    f <- fitted(mod)[, 1]
    y <- obs#as.numeric(obs) - 1
  } else {
    e <- residuals(mod)
    f <- fitted(mod)
    y <- as.numeric(obs) - 1
  }
  
  devr <- sign(e) * sqrt(-2 * (y * log(f) + (1 - y) * log(1 - f)))
  
  return(devr)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ---- MODEL ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set.seed(2024) 

#create train and test data
splitIndex <- createDataPartition(sin_generar$BIM6, p = 1, list = FALSE)
trainDataList <- sin_generar[splitIndex, ]
testDataList <- sin_generar[-splitIndex, ]

#priors
priors <- c(set_prior("normal(0, 10)", class = "b", coef = "ANTIGUEDAD"),
            set_prior("normal(0, 10)", class = "b", coef = "PROP_BI"))

#model
modelo <- brm(BIM6 ~ ANTIGUEDAD + PROP_BI, 
               data = trainDataList, prior = priors, family = bernoulli(),
               chains = 4, iter = 2000, warmup = 1000) 

summary(modelo) 

# verify priors
prior_summary(modelo, all=F)

# to extract fixed efects
fixef(modelo) 

# performance metrics
waic <- loo(modelo, cores = 4) 

#predictions and testing
predictions <- fitted(modelo, newdata = trainDataList, type = "response")[,1] %>% round(5)

predictedClass <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix <- table(Predicted = predictedClass, Actual = trainDataList$BIM6)
print(confusionMatrix)
print(paste('Error Global:', 
            (round((confusionMatrix[1,2] + confusionMatrix[2,1]) / sum(confusionMatrix),4)*100),
            "%"))
print(paste('Sensibilidad:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[1,2]),4)*100),
            "%"))
print(paste('Especificidad:', 
            (round((confusionMatrix[1,1]) / (confusionMatrix[1,1]+confusionMatrix[2,1]),4)*100),
            "%"))
print(paste('Precisión:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[2,1]),4)*100),
            "%"))

# summary print
tabla_latex <- xtable(as.data.frame(summary(modelo)$fixed), 
                       caption = "Resumen de Modelo", display = c("s","f","f","f","f","f","f","f"), 
                       digits = c(0, 3, 3, 3, 3, 3, 0, 0))
print(tabla_latex, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')


# diagnostico
color_scheme_set("red")
mcmc_areas(modelo, pars = c("b_Intercept"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_areas(modelo, pars = c("b_ANTIGUEDAD", "b_PROP_BI"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_trace(modelo, pars = c("b_Intercept", "b_ANTIGUEDAD", "b_PROP_BI"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ----- spatial incorporation ------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### ---- PADRON ----

# distancias al vecino mas cercano de cada observacion
centro <- st_centroid(st_geometry(sin_generar))
nb1 <- knn2nb(knearneigh(centro, k = 1))
dist1 <- nbdists(nb1, centro)
summary(unlist(dist1))

ggplot(data = data.frame(dist = unlist(dist1)), aes(x = dist)) +
  geom_histogram(fill = "coral", color = "black", bins = 30) +
  labs(x = "Distancia en metros", y = "Frecuencia") +
  theme_minimal() +
  scale_y_continuous(
    labels = scales::comma_format(scale = 1),
    breaks =c(0, 50, 100, 150, 200, 250)) + 
  scale_x_continuous(
    labels = scales::comma_format(scale = 1),
    breaks =c(0, 100, 250, 500, 1000, 1250)) 



centroides <- st_centroid(trainDataList$geometry)
coordenadas <- st_coordinates(centroides)
W_2000 <- dnearneigh(coordenadas, d1 = 0, d2 = 1500, longlat = FALSE)

W_list <- nb2listw(W_2000, style = 'W', zero.policy = TRUE) # estandarizada

# residuos de devianza
residuos.d <- dres(modelo, obs=trainDataList$BIM6, brms=TRUE)

# con residuos de devianza
moran.test(residuos.d, W_list, zero.policy = TRUE) 
# p-valor = 0.005, hay correlacion espacial


### ---- CCZ ----

# leo un mapa con los CCZ de montevideo
ccz <- st_read('compute/data/ine_ccz_mvd.shp')
# y le asigno el crs de los carlitos
st_crs(ccz)<- st_crs(sin_generar)


# agrugapar observaciones en sin_genearr por ccz, y calcular la media de BIM6
trainDataList$residuos.d <- dres(modelo, obs=trainDataList$BIM6, brms=TRUE)
trainDataList$BIM6 <- as.numeric(trainDataList$BIM6) -1 
trainDataList$CCZ <- as.factor(trainDataList$CCZ)

# agrupar
trainDataList %>% 
  group_by(CCZ) %>% 
  summarise(mean_BIM6 = mean(BIM6, na.rm = TRUE), total = n()) -> mean_BIM6

trainDataList %>% 
  group_by(CCZ) %>% 
  summarise(mean_dres = mean(residuos.d, na.rm = TRUE), total = n()) -> mean_dres

trainDataList %>% 
  group_by(CCZ) %>% 
  summarise(median_dres = median(residuos.d, na.rm = TRUE), total = n()) -> median_dres

# crear variables mean_bim6 y mean_res.d en un mismo objeto
mean_BIM6 <- as.data.frame(mean_BIM6)[,c('CCZ','mean_BIM6')]
mean_dres <- as.data.frame(mean_dres)[,c('CCZ','mean_dres')]
median_dres <- as.data.frame(median_dres)[,c('CCZ','median_dres')]

ccz <- merge(ccz, mean_BIM6, by.x = 'CCZ', by.y = 'CCZ')
ccz <- merge(ccz, mean_dres, by.x = 'CCZ', by.y = 'CCZ')
ccz <- merge(ccz, median_dres, by.x = 'CCZ', by.y = 'CCZ')

# tmap con la media de los residuos por CCZ
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='mean_BIM6',
              #breaks = c(-0.15, 0, 0.1, 0.15, 0.2, 0.3, 0.45, 0.6),
              title = "Media BIM6") + 
  tm_text("CCZ", size = 1.5)

# crear matriz de pesos
Wccz <- nb2listw(poly2nb(ccz), style = 'B', zero.policy = TRUE)
WcczW <- nb2listw(poly2nb(ccz), style = 'W', zero.policy = TRUE)

# moran test de la media de BIM6 por CCZ
moran.test(x = ccz$mean_BIM6, listw = Wccz)
# p-valor 0.0385 hay dependencia espacial
moran.test(x = ccz$mean_BIM6, listw = WcczW)
# p-valor 0.0876 no hay dependencia espacial

# tmap con la media de los residuos por CCZ
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='mean_dres',
              breaks = c(-0.15, 0, 0.1, 0.15, 0.2, 0.3, 0.45, 0.6),
              title = "Residuos de Devianza") + 
  tm_text("CCZ", size = 1.5)

#  moran test de la media de los residuos
moran.test(ccz$mean_dres, listw = Wccz) # se utilizo el totalidad de la muestra.
# p-valor 0.210 no hay dependencia espacial con la media de los residuos de devianza agregados por ccz
moran.test(ccz$mean_dres, listw = WcczW) # se utilizo el totalidad de la muestra.
# p-valor 0.258 no hay dependencia espacial con la media de los residuos de devianza agregados por ccz


# tmap con la mediana de los residuos por CCZ
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='median_dres',
              breaks = c(-0.15, 0, 0.3, 0.45, 0.6, 0.9, 1.2),
              title = "Residuos de Devianza") + 
  tm_text("CCZ", size = 1.5)

# moran test de la mediana de los residuos
moran.test(ccz$median_dres, listw = Wccz)
# p-valor 0.0385 hay dependencia espacial, se rechaza H0
moran.test(ccz$median_dres, listw = WcczW)
# p-valor 0.0695 hay dependencia espacial, se rechaza H0





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## ----- complementario -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# se estudia la autocorrelacion espcial para el modelo GLM 

trainDataList <- sin_generar[splitIndex, ]

# glm model
modelo_glm <- glm(BIM6 ~ ANTIGUEDAD + PROP_BI, data = trainDataList, family = binomial)

trainDataList$residuos.dglm <- dres(modelo_glm, obs=trainDataList$BIM6, brms=FALSE)
trainDataList$BIM6 <- as.numeric(trainDataList$BIM6) -1 
trainDataList$CCZ <- as.factor(trainDataList$CCZ)

# agrupar
trainDataList %>% 
  group_by(CCZ) %>% 
  summarise(mean_BIM6 = mean(BIM6, na.rm = TRUE), total = n()) -> mean_BIM6

trainDataList %>% 
  group_by(CCZ) %>% 
  summarise(mean_dres = mean(residuos.dglm, na.rm = TRUE), total = n()) -> mean_dres

trainDataList %>% 
  group_by(CCZ) %>% 
  summarise(median_dres = median(residuos.dglm, na.rm = TRUE), total = n()) -> median_dres

# crear variables mean_bim6 y mean_res.d en un mismo objeto
mean_BIM6 <- as.data.frame(mean_BIM6)[,c('CCZ','mean_BIM6')]
mean_dres <- as.data.frame(mean_dres)[,c('CCZ','mean_dres')]
median_dres <- as.data.frame(median_dres)[,c('CCZ','median_dres')]

ccz <- st_read('compute/data/ine_ccz_mvd.shp')
st_crs(ccz)<- st_crs(sin_generar)

ccz <- merge(ccz, mean_BIM6, by.x = 'CCZ', by.y = 'CCZ')
ccz <- merge(ccz, mean_dres, by.x = 'CCZ', by.y = 'CCZ')
ccz <- merge(ccz, median_dres, by.x = 'CCZ', by.y = 'CCZ')

# tmap con la media de los resoiduos por CCZ
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='mean_dres',
              breaks = c(-0.15, 0, 0.1, 0.15, 0.2, 0.3, 0.45, 0.6),
              title = "Residuos de Devianza") + 
  tm_text("CCZ", size = 1.5)

# crear matriz de pesos
Wccz <- nb2listw(poly2nb(ccz), style = 'B', zero.policy = TRUE)
WcczW <- nb2listw(poly2nb(ccz), style = 'W', zero.policy = TRUE)

# moran test de la media de BIM6 por CCZ
moran.test(x = ccz$mean_BIM6, listw = Wccz)
# p-valor 0.0564 no hay dependencia espacial
moran.test(x = ccz$mean_BIM6, listw = WcczW)
# p-valor 0.0876 no hay dependencia espacial

#  moran test de la media de los residuos
moran.test(ccz$mean_dres, listw = Wccz) # se utilizo el totalidad de la muestra.
# p-valor 0.208 no hay dependencia espacial con la media de los residuos de devianza agregados por ccz
moran.test(ccz$mean_dres, listw = WcczW) # se utilizo el totalidad de la muestra.
# p-valor 0.2566 no hay dependencia espacial con la media de los residuos de devianza agregados por ccz


# tmap con la mediana de los residuos por CCZ
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='median_dres',
              breaks = c(-0.15, 0, 0.3, 0.45, 0.6, 0.9, 1.2),
              title = "Residuos de Devianza") + 
  tm_text("CCZ", size = 1.5)

# moran test de la mediana de los residuos
moran.test(ccz$median_dres, listw = Wccz)
# p-valor 0.0368 hay dependencia espacial, se rechaza H0
moran.test(ccz$median_dres, listw = WcczW)
# p-valor 0.0675 hay dependencia espacial, se rechaza H0



# los resultados en el modelo GLM son casi identicos en cuanto a los test de autocorrelacion espacial
# en comparacion al modelo en BRMs




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## -------------- CAR --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Integramos al modelo el componente espacial car()
# brm() necesitar usar nb2mat

ccz <- st_read('compute/data/ine_ccz_mvd.shp')
st_crs(ccz)<- st_crs(sin_generar)

B_car <- nb2mat(poly2nb(ccz), style = "B", zero.policy = TRUE) 

# usamos todas las obeservaciones
splitIndex <- createDataPartition(sin_generar$BIM6, p = 1, list = FALSE)
trainDataList <- sin_generar[splitIndex, ]

### ---- CAR ----

time <- Sys.time()
mod_Bcar <- brm(BIM6 ~ ANTIGUEDAD + PROP_BI + car(B_car, gr = CCZ, type = 'escar'),
               data = trainDataList, 
               data2 = list(B_car = B_car),
               prior = priors,
               family = bernoulli(),
               iter = 4500,  
               warmup = 2250,
               chains = 4,
               cores = 4)
final <- Sys.time() - time
print(final)

summary(mod_Bcar)

# print table for document
tabla_latex_car <- xtable(as.data.frame(rbind(summary(mod_Bcar)$fixed,summary(mod_Bcar)$cor_pars)), 
                          caption = "Resumen de Modelo", display = c("s","f","f","f","f","f","f","f"), 
                          digits = c(0, 3, 3, 3, 3, 3, 0, 0))
print(tabla_latex_car, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')

# predictions metrics
predictions <- fitted(mod_Bcar, newdata = trainDataList, type = "response")[,1] %>% round(5)

predictedClass <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix <- table(Predicted = predictedClass, Actual = trainDataList$BIM6)
print(confusionMatrix)
print(paste('Error Global:', 
            (round((confusionMatrix[1,2] + confusionMatrix[2,1]) / sum(confusionMatrix),4)*100),
            "%"))
print(paste('Sensibilidad:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[1,2]),4)*100),
            "%"))
print(paste('Especificidad:', 
            (round((confusionMatrix[1,1]) / (confusionMatrix[1,1]+confusionMatrix[2,1]),4)*100),
            "%"))
print(paste('Precisión:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[2,1]),4)*100),
            "%"))

# diagnostico
color_scheme_set("red")
mcmc_areas(mod_Bcar, pars = c("b_Intercept"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_areas(mod_Bcar, pars = c("b_ANTIGUEDAD", "b_PROP_BI"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_areas(mod_Bcar, pars =c("car", "sdcar"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_areas(mod_Bcar, pars = c("rcar[2]", "rcar[4]", "rcar[9]"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_areas(mod_Bcar, pars = c("car"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_trace(mod_Bcar, pars = c("b_Intercept", "b_ANTIGUEDAD", "b_PROP_BI"))
mcmc_trace(mod_Bcar, pars = c("car", "sdcar"))
mcmc_trace(mod_Bcar, pars = c("rcar[2]", "rcar[4]", "rcar[9]"))

# waic
waic1_Bcar <- loo(mod_Bcar, cores = 4)
waic1_Bcar

# test de moran
trainDataList$residuos.dcar <- dres(mod_Bcar, obs=trainDataList$BIM6, brms=TRUE)

# agrupar
trainDataList %>% group_by(CCZ) %>% 
  summarise(median_dres = median(residuos.dcar, na.rm = TRUE), total = n()) -> median_dres

# crear variable mean_res.d en un mismo objeto
median_dres <- as.data.frame(median_dres)[,c('CCZ','median_dres')]
ccz <- merge(ccz, median_dres, by.x = 'CCZ', by.y = 'CCZ')

# crear matriz de pesos
Wccz <- nb2listw(poly2nb(ccz), style = 'B', zero.policy = TRUE)
WcczW <- nb2listw(poly2nb(ccz), style = 'W', zero.policy = TRUE)

# tmap con la mediana de los residuos por CCZ
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='median_dres',
              #breaks = c(-0.15, 0, 0.3, 0.45, 0.6, 0.9, 1.2),
              title = "Residuos de Devianza") + 
  tm_text("CCZ", size = 1.5)

# moran test de la mediana de los residuos
moran.test(ccz$median_dres, listw = Wccz)
# p-valor 0.0497 hay dependencia espacial, se rechaza H0 ; antes p-valor 0.0385
moran.test(ccz$median_dres, listw = WcczW)
# p-valor 0.0814 hay ligera dependencia espacial, se rechaza H0 ; antes p-valor 0.0695

# prediction
ed <- prepare_predictions(mod_Bcar)

# valores de los efectos aleatorios de los CCZ
phi = apply(ed$dpars$mu$ac$rcar,2,mean)
tab <- data.frame(ccz=c(12,9,13,10,11,14,18,15,17,3,16,8,6,4,7,2,1,5),phi=as.vector(phi))
phi <- tab[order(tab$ccz),]
phi_odds <- cbind(phi,exp(phi$phi)-1)

tabla_latex_phicar <- xtable(phi_odds, 
                            caption = "Resumen de Modelo", display = c("s","f","f", "f"), 
                            digits = c(0, 0, 4, 4))
print(tabla_latex_phicar, type = "latex", include.rownames = FALSE, comment = FALSE, caption.placement='top')

# merge with ccz to plot
ccz <- merge(ccz, phi_odds, by.x = 'CCZ', by.y = 'ccz')

# grafico de los efectos aleatorios con tmap
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='exp(phi$phi) - 1',
              breaks = c(-.4,-.3,-.2,-.1, 0, .1, .2, .3, .4, .5, .6),
              title = "Efectos Aleatorios",
              palette = c(
                "-0.4 to -0.3" = "#AE123A",
                "-0.3 to -0.2" = "#E03A42", 
                "-0.2 to -0.1" = "#FA8D76", 
                "-0.1 to 0.0" = "#FFBEB2",
                "0.0 to 0.1" = "#B3E0A6",
                "0.1 to 0.2" = "#92D282",
                "0.2 to 0.3" = "#7BC16E",
                "0.3 to 0.4" = "#60A855",
                "0.4 to 0.5" = "#358747",
                "0.5 to 0.6" = "#256D3D"),
              midpoint = 0) + 
  tm_text("CCZ", size = 1.5)


### ---- ICAR ----

set.seed(2014)

time_esicar <- Sys.time()
mod_Besicar <- brm(BIM6 ~ ANTIGUEDAD + PROP_BI + car(B_car, gr = CCZ, type = 'esicar'),
               data = trainDataList, 
               data2 = list(B_car = B_car), 
               prior = priors,
               family = bernoulli(),
               iter = 7000,  
               warmup = 3500,
               chains = 4,
               cores = 4)
final_esicar <- Sys.time() - time_esicar
print(final_esicar)

summary(mod_Besicar)

# print table for document
tabla_latex_icar <- xtable(as.data.frame(rbind(summary(mod_Besicar)$fixed,summary(mod_Besicar)$cor_pars)), 
                          caption = "Resumen de Modelo", display = c("s","f","f","f","f","f","f","f"), 
                          digits = c(0, 3, 3, 3, 3, 3, 0, 0))
print(tabla_latex_icar, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')

# predictions metrics
predictions <- fitted(mod_Besicar, newdata = trainDataList, type = "response")[,1] %>% round(5)

predictedClass <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix <- table(Predicted = predictedClass, Actual = trainDataList$BIM6)
print(confusionMatrix)
print(paste('Error Global:', 
            (round((confusionMatrix[1,2] + confusionMatrix[2,1]) / sum(confusionMatrix),4)*100),
            "%"))
print(paste('Sensibilidad:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[1,2]),4)*100),
            "%"))
print(paste('Especificidad:', 
            (round((confusionMatrix[1,1]) / (confusionMatrix[1,1]+confusionMatrix[2,1]),4)*100),
            "%"))
print(paste('Precisión:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[2,1]),4)*100),
            "%"))

# diagnostico
color_scheme_set("red")
mcmc_areas(mod_Besicar, pars = c("b_Intercept"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_areas(mod_Besicar, pars = c("b_ANTIGUEDAD", "b_PROP_BI"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_areas(mod_Besicar, pars =c("sdcar"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_areas(mod_Besicar, pars = c("rcar[2]", "rcar[4]", "rcar[9]"), prob = 0.95, prob_outer = 0.99, point_est = "mean")
mcmc_trace(mod_Besicar, pars = c("b_Intercept", "b_ANTIGUEDAD", "b_PROP_BI"))
mcmc_trace(mod_Besicar, pars = c("sdcar"))
mcmc_trace(mod_Besicar, pars = c("rcar[2]", "rcar[4]", "rcar[9]"))

# waic
waic2_esicar <- loo(mod_Besicar, cores = 4)
waic2_esicar

# test de moran
trainDataList$residuos.desicar <- dres(mod_Besicar, obs=trainDataList$BIM6, brms=TRUE)

# agrupar
trainDataList %>% group_by(CCZ) %>% 
  summarise(median_dres2 = median(residuos.desicar, na.rm = TRUE), total = n()) -> median_dres2

# crear variable mean_res.d en un mismo objeto
median_dres2 <- as.data.frame(median_dres2)[,c('CCZ','median_dres2')]
ccz <- merge(ccz, median_dres2, by.x = 'CCZ', by.y = 'CCZ')

# tmap con la mediana de los residuos por CCZ
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='median_dres2',
              #breaks = c(-0.15, 0, 0.3, 0.45, 0.6, 0.9, 1.2),
              title = "Residuos de Devianza") + 
  tm_text("CCZ", size = 1.5)

# moran test de la mediana de los residuos
moran.test(ccz$median_dres2, listw = Wccz)
# p-valor 0.0410 hay dependencia espacial, se rechaza H0 ; antes p-valor 0.0497
moran.test(ccz$median_dres2, listw = WcczW)
# p-valor 0.0673 hay ligera dependencia espacial, se rechaza H0 ; antes p-valor 0.0814



# prediction
ed2 <- prepare_predictions(mod_Besicar)

# valores de los efectos aleatorios de los CCZ
phi2 = apply(ed2$dpars$mu$ac$rcar,2,mean)
tab2 <- data.frame(ccz=c(12,9,13,10,11,14,18,15,17,3,16,8,6,4,7,2,1,5),phi=as.vector(phi))
phi2 <- tab[order(tab$ccz),]
phi2_odds <- cbind(phi2,exp(phi2$phi)-1)

# media absoluta de los efectos aleatorios
mean(abs(phi$phi))

tabla_latex_phi2car <- xtable(phi2_odds, 
                             caption = "Resumen de Modelo", display = c("s","f","f", "f"), 
                             digits = c(0, 0, 4, 4))
print(tabla_latex_phi2car, type = "latex", include.rownames = FALSE, comment = FALSE, caption.placement='top')

# merge with ccz to plot
ccz <- merge(ccz, phi2_odds, by.x = 'CCZ', by.y = 'ccz')

# grafico de los efectos aleatorios con tmap
tmap_mode("view")
tm_shape(ccz) + 
  tm_polygons(col='exp(phi2$phi) - 1',
              breaks = c(-.4,-.3,-.2,-.1, 0, .1, .2, .3, .4, .5, .6),
              title = "Efectos Aleatorios",
              palette = c(
                "-0.4 to -0.3" = "#AE123A",
                "-0.3 to -0.2" = "#E03A42", 
                "-0.2 to -0.1" = "#FA8D76", 
                "-0.1 to 0.0" = "#FFBEB2",
                "0.0 to 0.1" = "#B3E0A6",
                "0.1 to 0.2" = "#92D282",
                "0.2 to 0.3" = "#7BC16E",
                "0.3 to 0.4" = "#60A855",
                "0.4 to 0.5" = "#358747",
                "0.5 to 0.6" = "#256D3D"),
              midpoint = 0) + 
  tm_text("CCZ", size = 1.5)


# compare with loo waic
loo::waic(mod_Bcar,mod_Besicar)






