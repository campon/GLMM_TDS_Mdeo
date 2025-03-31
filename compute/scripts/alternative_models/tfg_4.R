# ahora repetir todo el final de modelos de tfg_3
# pero que los modelos por CCZ y completo no incluyan la variable explicativa BIM5
# la idea de comparar esto con los modelos anteriores es por el hecho de que
# BIM5 es una variable explicativa tan relevante para el modelo de tal manera que 
# determina la variable de respuesta, la idea de probar replicar estos modelo sin
# BIM5 es comparar los errores globales por CCZ y en el modelo completo
# para ver que tan importante es su ausencia en el modelo para explicar BIM6.

library(sf)
library(tidyverse)
library(brms)
library(pROC)


# lectura de TDS totales
dataSP3 <- st_read("TFG/archivos/dataSP3.shp") # 1 min en cargar

dataSP3

# construyo la variable antiguedad a partir de F_APERTURA
dataSP3 <- dataSP3  %>% mutate(ANTIGUEDAD = (as.Date("2022-12-23") - F_APERT)/365.25)

# transformacion de variables
dataSP3$CATEGOR <- as.factor(dataSP3$CATEGOR)

dataSP3$BIM6 <- as.factor(dataSP3$BIM6)

dataSP3$BIM5 <- as.factor(dataSP3$BIM5)

dataSP3$TS_CORT <- as.factor(dataSP3$TS_CORT)

summary(dataSP3) # 251 NA's en CANT_BIM_IMPAGOS

# dejando de lado los NA , intentar resolver en IM porque esas cuentas tienen NA

# brm pide que la variable de respuesta y sea nuemerica, entonces
dataSP3$BIM6 <- ifelse(dataSP3$BIM6=='impago',0,1)
table(dataSP3$BIM6)

# chequeos
#which(!(st_is_valid(dataSP3))) # son las observaciones [184024, 233667]
# solucionamos
dataSP3[c(184024, 233667),] <- st_make_valid(dataSP3[c(184024, 233667),]) 


# los datos por ccz
ccz1 <- dataSP3 %>% filter(CCZ == '1')
ccz2 <- dataSP3 %>% filter(CCZ == '2')
ccz3 <- dataSP3 %>% filter(CCZ == '3')
ccz4 <- dataSP3 %>% filter(CCZ == '4')
ccz5 <- dataSP3 %>% filter(CCZ == '5')
ccz6 <- dataSP3 %>% filter(CCZ == '6')
ccz7 <- dataSP3 %>% filter(CCZ == '7')
ccz8 <- dataSP3 %>% filter(CCZ == '8')
ccz9 <- dataSP3 %>% filter(CCZ == '9')
ccz10 <- dataSP3 %>% filter(CCZ == '10')
ccz11 <- dataSP3 %>% filter(CCZ == '11')
ccz12 <- dataSP3 %>% filter(CCZ == '12')
ccz13 <- dataSP3 %>% filter(CCZ == '13')
ccz14 <- dataSP3 %>% filter(CCZ == '14')
ccz15 <- dataSP3 %>% filter(CCZ == '15')
ccz16 <- dataSP3 %>% filter(CCZ == '16')
ccz17 <- dataSP3 %>% filter(CCZ == '17')
ccz18 <- dataSP3 %>% filter(CCZ == '18')


# los modelos brm
ahora1 <- Sys.time()
mod5_1 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz1, family = binomial() ) 
ahora1 <- Sys.time() - ahora1

ahora2 <- Sys.time()
mod5_2 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz2, family = binomial() ) 
ahora2 <- Sys.time() - ahora2 

ahora3 <- Sys.time()
mod5_3 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz3, family = binomial() ) 
ahora3 <- Sys.time() - ahora3

ahora4 <- Sys.time()
mod5_4 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz4, family = binomial() ) 
ahora4 <- Sys.time() - ahora4

ahora5 <- Sys.time()
mod5_5 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz5, family = binomial() ) 
ahora5 <- Sys.time() - ahora5 

ahora6 <- Sys.time()
mod5_6 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz6, family = binomial() ) 
ahora6 <- Sys.time() - ahora6

ahora7 <- Sys.time()
mod5_7 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz7, family = binomial() ) 
ahora7 <- Sys.time() - ahora7

ahora8 <- Sys.time()
mod5_8 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz8, family = binomial() ) 
ahora8 <- Sys.time() - ahora8

ahora9 <- Sys.time()
mod5_9 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz9, family = binomial() ) 
ahora9 <- Sys.time() - ahora9

ahora10 <- Sys.time()
mod5_10 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz10, family = binomial() ) 
ahora10 <- Sys.time() - ahora10

ahora11 <- Sys.time()
mod5_11 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz11, family = binomial() ) 
ahora11 <- Sys.time() - ahora11

ahora12 <- Sys.time()
mod5_12 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz12, family = binomial() ) 
ahora12 <- Sys.time() - ahora12

ahora13 <- Sys.time()
mod5_13 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz13, family = binomial() ) 
ahora13 <- Sys.time() - ahora13

ahora14 <- Sys.time()
mod5_14 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz14, family = binomial() ) 
ahora14 <- Sys.time() - ahora14

ahora15 <- Sys.time()
mod5_15 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz15, family = binomial() ) 
ahora15 <- Sys.time() - ahora15

ahora16 <- Sys.time()
mod5_16 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz16, family = binomial() ) 
ahora16 <- Sys.time() - ahora16

ahora17 <- Sys.time()
mod5_17 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz17, family = binomial() ) 
ahora17 <- Sys.time() - ahora17

ahora18 <- Sys.time()
mod5_18 <- brm(BIM6 ~ CATEGOR  + CANT_BI + CANT_UN + ANTIGUEDAD, data = ccz18, family = binomial() ) 
ahora18 <- Sys.time() - ahora18


# los coeficientes
c5_1 <- posterior_summary(mod5_1)[,1] %>%round(5)
c5_2 <- posterior_summary(mod5_2)[,1] %>%round(5)
c5_3 <- posterior_summary(mod5_3)[,1] %>%round(5)
c5_4 <- posterior_summary(mod5_4)[,1] %>%round(5)
c5_5 <- posterior_summary(mod5_5)[,1] %>%round(5)
c5_6 <- posterior_summary(mod5_6)[,1] %>%round(5)
c5_7 <- posterior_summary(mod5_7)[,1] %>%round(5)
c5_8 <- posterior_summary(mod5_8)[,1] %>%round(5)
c5_9 <- posterior_summary(mod5_9)[,1] %>%round(5)
c5_10 <- posterior_summary(mod5_10)[,1] %>%round(5)
c5_11 <- posterior_summary(mod5_11)[,1] %>%round(5)
c5_12 <- posterior_summary(mod5_12)[,1] %>%round(5)
c5_13 <- posterior_summary(mod5_13)[,1] %>%round(5)
c5_14 <- posterior_summary(mod5_14)[,1] %>%round(5)
c5_15 <- posterior_summary(mod5_15)[,1] %>%round(5)
c5_16 <- posterior_summary(mod5_16)[,1] %>%round(5)
c5_17 <- posterior_summary(mod5_17)[,1] %>%round(5)
c5_18 <- posterior_summary(mod5_18)[,1] %>%round(5)

# intervalos inferior
ic5_1 <- posterior_summary(mod5_1)[,3] %>%round(5)
ic5_2 <- posterior_summary(mod5_2)[,3] %>%round(5)
ic5_3 <- posterior_summary(mod5_3)[,3] %>%round(5)
ic5_4 <- posterior_summary(mod5_4)[,3] %>%round(5)
ic5_5 <- posterior_summary(mod5_5)[,3] %>%round(5)
ic5_6 <- posterior_summary(mod5_6)[,3] %>%round(5)
ic5_7 <- posterior_summary(mod5_7)[,3] %>%round(5)
ic5_8 <- posterior_summary(mod5_8)[,3] %>%round(5)
ic5_9 <- posterior_summary(mod5_9)[,3] %>%round(5)
ic5_10 <- posterior_summary(mod5_10)[,3] %>%round(5)
ic5_11 <- posterior_summary(mod5_11)[,3] %>%round(5)
ic5_12 <- posterior_summary(mod5_12)[,3] %>%round(5)
ic5_13 <- posterior_summary(mod5_13)[,3] %>%round(5)
ic5_14 <- posterior_summary(mod5_14)[,3] %>%round(5)
ic5_15 <- posterior_summary(mod5_15)[,3] %>%round(5)
ic5_16 <- posterior_summary(mod5_16)[,3] %>%round(5)
ic5_17 <- posterior_summary(mod5_17)[,3] %>%round(5)
ic5_18 <- posterior_summary(mod5_18)[,3] %>%round(5)

# intervalos superior
ics5_1 <- posterior_summary(mod5_1)[,4] %>%round(5)
ics5_2 <- posterior_summary(mod5_2)[,4] %>%round(5)
ics5_3 <- posterior_summary(mod5_3)[,4] %>%round(5)
ics5_4 <- posterior_summary(mod5_4)[,4] %>%round(5)
ics5_5 <- posterior_summary(mod5_5)[,4] %>%round(5)
ics5_6 <- posterior_summary(mod5_6)[,4] %>%round(5)
ics5_7 <- posterior_summary(mod5_7)[,4] %>%round(5)
ics5_8 <- posterior_summary(mod5_8)[,4] %>%round(5)
ics5_9 <- posterior_summary(mod5_9)[,4] %>%round(5)
ics5_10 <- posterior_summary(mod5_10)[,4] %>%round(5)
ics5_11 <- posterior_summary(mod5_11)[,4] %>%round(5)
ics5_12 <- posterior_summary(mod5_12)[,4] %>%round(5)
ics5_13 <- posterior_summary(mod5_13)[,4] %>%round(5)
ics5_14 <- posterior_summary(mod5_14)[,4] %>%round(5)
ics5_15 <- posterior_summary(mod5_15)[,4] %>%round(5)
ics5_16 <- posterior_summary(mod5_16)[,4] %>%round(5)
ics5_17 <- posterior_summary(mod5_17)[,4] %>%round(5)
ics5_18 <- posterior_summary(mod5_18)[,4] %>%round(5)


# dataframe 5_mods del 3 coeficiente (CANT_BIM)
datos5 <- data.frame(ccz  = as.factor(1:18),
                     beta = c(c5_1[3],c5_2[3],c5_3[3],c5_4[3],c5_5[3],c5_6[3],c5_7[3],c5_8[3],c5_9[3],c5_10[3],
                              c5_11[3],c5_12[3],c5_13[3],c5_14[3],c5_15[3],c5_16[3],c5_17[3],c5_18[3]),
                     or   = exp(c(c5_1[3],c5_2[3],c5_3[3],c5_4[3],c5_5[3],c5_6[3],c5_7[3],c5_8[3],c5_9[3],c5_10[3],
                                  c5_11[3],c5_12[3],c5_13[3],c5_14[3],c5_15[3],c5_16[3],c5_17[3],c5_18[3])),
                     linf95 = c(ic5_1[3],ic5_2[3],ic5_3[3],ic5_4[3],ic5_5[3],ic5_6[3],ic5_7[3],ic5_8[3],ic5_9[3],ic5_10[3],
                                ic5_11[3],ic5_12[3],ic5_13[3],ic5_14[3],ic5_15[3],ic5_16[3],ic5_17[3],ic5_18[3]),
                     lsup95 = c(ics5_1[3],ics5_2[3],ics5_3[3],ics5_4[3],ics5_5[3],ics5_6[3],ics5_7[3],ics5_8[3],ics5_9[3],ics5_10[3],
                                ics5_11[3],ics5_12[3],ics5_13[3],ics5_14[3],ics5_15[3],ics5_16[3],ics5_17[3],ics5_18[3])
)

# los siguientes graficos son para CANT_BIM

ggplot(datos5, aes(x = beta, y = ccz, col = ccz)) +
  geom_point() +
  geom_errorbar(aes(xmin = linf95, xmax = lsup95), width = 0.2) +
  xlab('Estimacion') +
  ylab('CCZ') +
  theme(text = element_text(size = 15)) +
  geom_vline(xintercept=0, linetype='dotted', color='blue', size=1.5)

# y en la escala del odds-ratio
ggplot(datos5, aes(x = or, y = ccz, col = ccz)) +
  geom_point() +
  geom_errorbar(aes(xmin = exp(linf95), xmax = exp(lsup95)), width = 0.2) +
  xlab('OR') +
  ylab('CCZ') +
  theme(text = element_text(size = 15)) +
  geom_vline(xintercept=1, linetype='dotted', color='blue', size=1.5)

# OR de 0.9 implica una disminucion del 10% en la prob de pago de BIM6 cuando se incrementa 1 en una unidad
# la cantidad de bimestres impagos.

# para sacar los predict
probs_1 <- fitted(mod5_1)
probs_2 <- fitted(mod5_2)
probs_3 <- fitted(mod5_3)
probs_4 <- fitted(mod5_4)
probs_5 <- fitted(mod5_5)
probs_6 <- fitted(mod5_6)
probs_7 <- fitted(mod5_7)
probs_8 <- fitted(mod5_8)
probs_9 <- fitted(mod5_9)
probs_10 <- fitted(mod5_10)
probs_11 <- fitted(mod5_11)
probs_12 <- fitted(mod5_12)
probs_13 <- fitted(mod5_13)
probs_14 <- fitted(mod5_14)
probs_15 <- fitted(mod5_15)
probs_16 <- fitted(mod5_16)
probs_17 <- fitted(mod5_17)
probs_18 <- fitted(mod5_18)



# intentar selecionar umbral que minimice el error global en la matriz de confusion que compara yobs , yhat

minimizo <- function(i=19,yobs,probs){
  umb <- 1/(i+1)
  cv <- rep(NA,i)
  
  for (j in 1:i){
    yhat <- ifelse(probs[,1] > umb, 1, 0)
    t1 <- table(yobs , yhat)
    if (ncol(t1)==1)
      break
    cv[j] <- (t1[1,2]+t1[2,1])/sum(t1)
    umb <- umb + 1/(1+i)
  }
  kk <- seq(1/(1+i), by=1/(1+i), length=i)
  posiciones <- which(cv == min(cv,na.rm=T))
  
  
  plot(kk, cv, pch=16, xlab='Umbrales', ylim = c(0,1), ylab='Error Global', main = 'Error de clasificacion') + abline(h=min(cv,na.rm = T),v=kk[posiciones])
  
  return(kk[posiciones])
}

# umbrales minimos por ccz: intervalos
minimizo(yobs=ccz1$BIM6[!is.na(ccz1$CANT_BI)],probs = probs_1) #{.75}
minimizo(yobs=ccz2$BIM6[!is.na(ccz2$CANT_BI)],probs = probs_2) #{.75}
minimizo(yobs=ccz3$BIM6[!is.na(ccz3$CANT_BI)],probs = probs_3) #{.75}
minimizo(yobs=ccz4$BIM6[!is.na(ccz4$CANT_BI)],probs = probs_4) #{.8}
minimizo(yobs=ccz5$BIM6[!is.na(ccz5$CANT_BI)],probs = probs_5) #{.8}
minimizo(yobs=ccz6$BIM6[!is.na(ccz6$CANT_BI)],probs = probs_6) #{.7}
minimizo(yobs=ccz7$BIM6[!is.na(ccz7$CANT_BI)],probs = probs_7) #{.8}
minimizo(yobs=ccz8$BIM6[!is.na(ccz8$CANT_BI)],probs = probs_8) #{.75}
minimizo(yobs=ccz9$BIM6[!is.na(ccz9$CANT_BI)],probs = probs_9) #{.65}
minimizo(yobs=ccz10$BIM6[!is.na(ccz10$CANT_BI)],probs = probs_10) #{.65}
minimizo(yobs=ccz11$BIM6[!is.na(ccz11$CANT_BI)],probs = probs_11) #{.6}
minimizo(yobs=ccz12$BIM6[!is.na(ccz12$CANT_BI)],probs = probs_12) #{.6}
minimizo(yobs=ccz13$BIM6[!is.na(ccz13$CANT_BI)],probs = probs_13) #{.65}
minimizo(yobs=ccz14$BIM6[!is.na(ccz14$CANT_BI)],probs = probs_14) #{.65}
minimizo(yobs=ccz15$BIM6[!is.na(ccz15$CANT_BI)],probs = probs_15) #{.75}
minimizo(yobs=ccz16$BIM6[!is.na(ccz16$CANT_BI)],probs = probs_16) #{.7}
minimizo(yobs=ccz17$BIM6[!is.na(ccz17$CANT_BI)],probs = probs_17) #{.65}
minimizo(yobs=ccz18$BIM6[!is.na(ccz18$CANT_BI)],probs = probs_18) #{.75}


# t1: en las filas es la realidad, el modelo predice que pagan 6672 + 51 pero en realidad pagan 6672 y 51 no pagaron, en cambio predice que 1127 no pagan y cree que 495 van a pagar pero en realidad no pagan


# poner los umbrales de cada CCZ que me dio la funcion minimizo
umb.1 <- .75
umb.2 <- .75
umb.3 <- .75
umb.4 <- .8
umb.5 <- .8
umb.6 <- .7
umb.7 <- .8
umb.8 <- .75
umb.9 <- .65
umb.10 <- .65
umb.11 <- .6
umb.12 <- .6
umb.13 <- .65
umb.14 <- .65
umb.15 <- .75
umb.16 <- .7
umb.17 <- .65
umb.18 <- .75

# luego los yhat de cada modeloCCZ 
yhat.1 <- ifelse(probs_1[,1] > umb.1, 1, 0)
yhat.2 <- ifelse(probs_2[,1] > umb.2, 1, 0)
yhat.3 <- ifelse(probs_3[,1] > umb.3, 1, 0)
yhat.4 <- ifelse(probs_4[,1] > umb.4, 1, 0)
yhat.5 <- ifelse(probs_5[,1] > umb.5, 1, 0)
yhat.6 <- ifelse(probs_6[,1] > umb.6, 1, 0)
yhat.7 <- ifelse(probs_7[,1] > umb.7, 1, 0)
yhat.8 <- ifelse(probs_8[,1] > umb.8, 1, 0)
yhat.9 <- ifelse(probs_9[,1] > umb.9, 1, 0)
yhat.10 <- ifelse(probs_10[,1] > umb.10, 1, 0)
yhat.11 <- ifelse(probs_11[,1] > umb.11, 1, 0)
yhat.12 <- ifelse(probs_12[,1] > umb.12, 1, 0)
yhat.13 <- ifelse(probs_13[,1] > umb.13, 1, 0)
yhat.14 <- ifelse(probs_14[,1] > umb.14, 1, 0)
yhat.15 <- ifelse(probs_15[,1] > umb.15, 1, 0)
yhat.16 <- ifelse(probs_16[,1] > umb.16, 1, 0)
yhat.17 <- ifelse(probs_17[,1] > umb.17, 1, 0)
yhat.18 <- ifelse(probs_18[,1] > umb.18, 1, 0)


# Luego los yobs de cada modeloCCZ
yobs.1 <- ccz1$BIM6[!is.na(ccz1$CANT_BI)]
yobs.2 <- ccz2$BIM6[!is.na(ccz2$CANT_BI)]
yobs.3 <- ccz3$BIM6[!is.na(ccz3$CANT_BI)]
yobs.4 <- ccz4$BIM6[!is.na(ccz4$CANT_BI)]
yobs.5 <- ccz5$BIM6[!is.na(ccz5$CANT_BI)]
yobs.6 <- ccz6$BIM6[!is.na(ccz6$CANT_BI)]
yobs.7 <- ccz7$BIM6[!is.na(ccz7$CANT_BI)]
yobs.8 <- ccz8$BIM6[!is.na(ccz8$CANT_BI)]
yobs.9 <- ccz9$BIM6[!is.na(ccz9$CANT_BI)]
yobs.10 <- ccz10$BIM6[!is.na(ccz10$CANT_BI)]
yobs.11 <- ccz11$BIM6[!is.na(ccz11$CANT_BI)]
yobs.12 <- ccz12$BIM6[!is.na(ccz12$CANT_BI)]
yobs.13 <- ccz13$BIM6[!is.na(ccz13$CANT_BI)]
yobs.14 <- ccz14$BIM6[!is.na(ccz14$CANT_BI)]
yobs.15 <- ccz15$BIM6[!is.na(ccz15$CANT_BI)]
yobs.16 <- ccz16$BIM6[!is.na(ccz16$CANT_BI)]
yobs.17 <- ccz17$BIM6[!is.na(ccz17$CANT_BI)]
yobs.18 <- ccz18$BIM6[!is.na(ccz18$CANT_BI)]

# por ultimo cada tabla y error global de cada modeloCCZ
t.1 <- table(yobs.1,yhat.1)
cv.1 <- (t.1[1,2]+t.1[2,1])/sum(t.1)

t.2 <- table(yobs.2,yhat.2)
cv.2 <- (t.2[1,2]+t.2[2,1])/sum(t.2)

t.3 <- table(yobs.3,yhat.3)
cv.3 <- (t.3[1,2]+t.3[2,1])/sum(t.3)

t.4 <- table(yobs.4,yhat.4)
cv.4 <- (t.4[1,2]+t.4[2,1])/sum(t.4)

t.5 <- table(yobs.5,yhat.5)
cv.5 <- (t.5[1,2]+t.5[2,1])/sum(t.5)

t.6 <- table(yobs.6,yhat.6)
cv.6 <- (t.6[1,2]+t.6[2,1])/sum(t.6)

t.7 <- table(yobs.7,yhat.7)
cv.7 <- (t.7[1,2]+t.7[2,1])/sum(t.7)

t.8 <- table(yobs.8,yhat.8)
cv.8 <- (t.8[1,2]+t.8[2,1])/sum(t.8)

t.9 <- table(yobs.9,yhat.9)
cv.9 <- (t.9[1,2]+t.9[2,1])/sum(t.9)

t.10 <- table(yobs.10,yhat.10)
cv.10 <- (t.10[1,2]+t.10[2,1])/sum(t.10)

t.11 <- table(yobs.11,yhat.11)
cv.11 <- (t.11[1,2]+t.11[2,1])/sum(t.11)

t.12 <- table(yobs.12,yhat.12)
cv.12 <- (t.12[1,2]+t.12[2,1])/sum(t.12)

t.13 <- table(yobs.13,yhat.13)
cv.13 <- (t.13[1,2]+t.13[2,1])/sum(t.13)

t.14 <- table(yobs.14,yhat.14)
cv.14 <- (t.14[1,2]+t.14[2,1])/sum(t.14)

t.15 <- table(yobs.15,yhat.15)
cv.15 <- (t.15[1,2]+t.15[2,1])/sum(t.15)

t.16 <- table(yobs.16,yhat.16)
cv.16 <- (t.16[1,2]+t.16[2,1])/sum(t.16)

t.17 <- table(yobs.17,yhat.17)
cv.17 <- (t.17[1,2]+t.17[2,1])/sum(t.17)

t.18 <- table(yobs.18,yhat.18)
cv.18 <- (t.18[1,2]+t.18[2,1])/sum(t.18)

# finalmente sumamos las tablas de cada modeloCCZ para calcular un unico error global
t.t.umb <- t.1 + t.2 + t.3 + t.4 + t.5 + t.6 + t.7 + t.8 + t.9 + t.10 + t.11 + t.12 + t.13 + t.14 + t.15 + t.16 + t.17 + t.18
cv.t.umb <- (t.t.umb[1,2]+t.t.umb[2,1])/sum(t.t.umb)
# este error unico nos permite comparar la determinacion de umbrales especificos por CCZ
# con un unico umbral = .45 en el modelo completo, ver que tan diferente es este error





# modelo COMPLETO con CCZ como variable.

dataSP3$CCZ <- as.factor(dataSP3$CCZ)

ahora.t <- Sys.time()
mod.t <- brm(BIM6 ~ CATEGOR + CCZ + CANT_BI + CANT_UN + ANTIGUEDAD, data = dataSP3, family = binomial() )
ahora.t <- Sys.time() - ahora.t
# ahora.t = 5.729257 hours !!!!!!

# estimacion
c_t <- posterior_summary(mod.t)[,1] %>% round(5)
or_t <- (exp(c_t) - 1) * 100 # estos son los porcentajes, el CCZ07 parece ser el mejor pagador

# interalos
ic_t <- posterior_summary(mod.t)[,3] %>% round(5)
ics_t <- posterior_summary(mod.t)[,4] %>% round(5)

# probs
ahora.probs <- Sys.time()
probs_t <- fitted(mod.t) #demora en correr y conlleva mucho trabajo computacional
ahora.probs <- Sys.time() - ahora.probs

minimizo(yobs=dataSP3$BIM6[!is.na(dataSP3$CANT_BI)],probs = probs_t) # minimiza en 0.6

# hacemos lo mismo que para cada modeloCCZ
umb.generico <- 0.6
yhat.mod.t <- ifelse(probs_t[,1] > umb.generico, 1, 0)
yobs.mod.t <- dataSP3$BIM6[!is.na(dataSP3$CANT_BI)]

t.mod.t <- table(yobs.mod.t,yhat.mod.t)
cv.mod.t <- (t.mod.t[1,2]+t.mod.t[2,1])/sum(t.mod.t)

# plotear de alguna forma todos los errores globales por CCZ
# y comparar dichos errores globales completos

cv.t.umb #0.152905

cv.mod.t #0.1656458

(cv.mod.t - cv.t.umb) *100 # la diferencia del error global es 0.0127408 bastante minima



# indice de concordancia c (otra medida de error global)


# Calcular el índice de concordancia de c; cuanto mas proximo a 1 mejor
c_index <- roc(dataSP3$BIM6[!is.na(dataSP3$CANT_BI)], probs_t[,1])$auc

c_index # Area under the curve: 0.8244
