#respaldo  tfg_2 / 03_modelos



# construyo la variable antiguedad con la variable F_APERTURA
#dataSP3 <- dataSP3  %>% mutate(antiguedad = (as.Date("2022-12-23") - F_APERT)/365) # antiguedad en anios 


# se filtran TDS con BIM6 'sin generar' (-1)
dataSP2 <- dataSP2 %>% filter(BIM6!='-1') # precisamente 155 obs.



# los datos por ccz
ccz1 <- dataSP2 %>% filter(CCZ == '1')
ccz2 <- dataSP2 %>% filter(CCZ == '2')
ccz3 <- dataSP2 %>% filter(CCZ == '3')
ccz4 <- dataSP2 %>% filter(CCZ == '4')
ccz5 <- dataSP2 %>% filter(CCZ == '5')
ccz6 <- dataSP2 %>% filter(CCZ == '6')
ccz7 <- dataSP2 %>% filter(CCZ == '7')
ccz8 <- dataSP2 %>% filter(CCZ == '8')
ccz9 <- dataSP2 %>% filter(CCZ == '9')
ccz10 <- dataSP2 %>% filter(CCZ == '10')
ccz11 <- dataSP2 %>% filter(CCZ == '11')
ccz12 <- dataSP2 %>% filter(CCZ == '12')
ccz13 <- dataSP2 %>% filter(CCZ == '13')
ccz14 <- dataSP2 %>% filter(CCZ == '14')
ccz15 <- dataSP2 %>% filter(CCZ == '15')
ccz16 <- dataSP2 %>% filter(CCZ == '16')
ccz17 <- dataSP2 %>% filter(CCZ == '17')
ccz18 <- dataSP2 %>% filter(CCZ == '18')

# los modelos
mod1_1 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
              data = ccz1, family = binomial(link = 'logit'))
mod1_2 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
              data = ccz2, family = binomial(link = 'logit'))
mod1_3 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
              data = ccz3, family = binomial(link = 'logit'))
mod1_4 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
              data = ccz4, family = binomial(link = 'logit'))
mod1_5 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
              data = ccz5, family = binomial(link = 'logit'))
mod1_6 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
              data = ccz6, family = binomial(link = 'logit'))
mod1_7 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
              data = ccz7, family = binomial(link = 'logit'))
mod1_8 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
              data = ccz8, family = binomial(link = 'logit'))
mod1_9 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
              data = ccz9, family = binomial(link = 'logit'))
mod1_10 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
               data = ccz10, family = binomial(link = 'logit'))
mod1_11 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
               data = ccz11, family = binomial(link = 'logit'))
mod1_12 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
               data = ccz12, family = binomial(link = 'logit'))
mod1_13 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
               data = ccz13, family = binomial(link = 'logit'))
mod1_14 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
               data = ccz14, family = binomial(link = 'logit'))
mod1_15 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
               data = ccz15, family = binomial(link = 'logit'))
mod1_16 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
               data = ccz16, family = binomial(link = 'logit'))
mod1_17 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
               data = ccz17, family = binomial(link = 'logit'))
mod1_18 <- glm(BIM6 ~ CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + CANT_BI + CANT_UN + ANTIGUEDAD,
               data = ccz18, family = binomial(link = 'logit'))

# las estimaciones
c1 <- coef(mod1_1)
c2 <- coef(mod1_2)
c3 <- coef(mod1_3)
c4 <- coef(mod1_4)
c5 <- coef(mod1_5)
c6 <- coef(mod1_6)
c7 <- coef(mod1_7)
c8 <- coef(mod1_8)
c9 <- coef(mod1_9)
c10 <- coef(mod1_10)
c11 <- coef(mod1_11)
c12 <- coef(mod1_12)
c13 <- coef(mod1_13)
c14 <- coef(mod1_14)
c15 <- coef(mod1_15)
c16 <- coef(mod1_16)
c17 <- coef(mod1_17)
c18 <- coef(mod1_18)

# los intervalos
ic1 <- confint(mod1_1)
ic2 <- confint(mod1_2)
ic3 <- confint(mod1_3)
ic4 <- confint(mod1_4)
ic5 <- confint(mod1_5)
ic6 <- confint(mod1_6)
ic7 <- confint(mod1_7)
ic8 <- confint(mod1_8)
ic9 <- confint(mod1_9)
ic10 <- confint(mod1_10)
ic11 <- confint(mod1_11)
ic12 <- confint(mod1_12)
ic13 <- confint(mod1_13)
ic14 <- confint(mod1_14)
ic15 <- confint(mod1_15)
ic16 <- confint(mod1_16)
ic17 <- confint(mod1_17)
ic18 <- confint(mod1_18)

# dataframe for beta=CATEGORDOMICILIARIO
datos <- data.frame(ccz  = as.factor(1:18),
                    beta = c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2],c8[2],c9[2],c10[2],
                             c11[2],c12[2],c13[2],c14[2],c15[2],c16[2],c17[2],c18[2]),
                    or   = exp(c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2],c8[2],c9[2],c10[2],
                                 c11[2],c12[2],c13[2],c14[2],c15[2],c16[2],c17[2],c18[2])),
                    linf95 = c(ic1[2,1],ic2[2,1],ic3[2,1],ic4[2,1],ic5[2,1],ic6[2,1],ic7[2,1],ic8[2,1],ic9[2,1],
                               ic10[2,1],ic11[2,1],ic12[2,1],ic13[2,1],ic14[2,1],ic15[2,1],ic16[2,1],ic17[2,1],ic18[2,1]),
                    lsup95 = c(ic1[2,2],ic2[2,2],ic3[2,2],ic4[2,2],ic5[2,2],ic6[2,2],ic7[2,2],ic8[2,2],ic9[2,2],
                               ic10[2,2],ic11[2,2],ic12[2,2],ic13[2,2],ic14[2,2],ic15[2,2],ic16[2,2],ic17[2,2],ic18[2,2])
)


# los siguientes graficos son para CATEGORIA DOMICILIARIO, eso varia 

ggplot(datos, aes(x = beta, y = ccz, col = ccz)) +
  geom_point() +
  geom_errorbar(aes(xmin = linf95, xmax = lsup95), width = 0.2) +
  xlab('Estimacion') +
  ylab('CCZ') +
  theme(text = element_text(size = 15)) +
  geom_vline(xintercept=0, linetype='dotted', color='blue', size=1.5)

# y en la escala del odds-ratio
ggplot(datos, aes(x = or, y = ccz, col = ccz)) +
  geom_point() +
  geom_errorbar(aes(xmin = exp(linf95), xmax = exp(lsup95)), width = 0.2) +
  xlab('OR') +
  ylab('CCZ') +
  theme(text = element_text(size = 15)) +
  geom_vline(xintercept=1, linetype='dotted', color='blue', size=1.5)


# dataframe 2 
datos2 <- data.frame(ccz  = as.factor(1:18),
                     beta = c(c1[4],c2[4],c4[4],c4[4],c5[4],c6[4],c7[4],c8[4],c9[4],c10[4],
                              c11[4],c12[4],c14[4],c14[4],c15[4],c16[4],c17[4],c18[4]),
                     or   = exp(c(c1[4],c2[4],c4[4],c4[4],c5[4],c6[4],c7[4],c8[4],c9[4],c10[4],
                                  c11[4],c12[4],c14[4],c14[4],c15[4],c16[4],c17[4],c18[4])),
                     linf95 = c(ic1[4,1],ic2[4,1],ic4[4,1],ic4[4,1],ic5[4,1],ic6[4,1],ic7[4,1],ic8[4,1],ic9[4,1],
                                ic10[4,1],ic11[4,1],ic12[4,1],ic14[4,1],ic14[4,1],ic15[4,1],ic16[4,1],ic17[4,1],ic18[4,1]),
                     lsup95 = c(ic1[4,2],ic2[4,2],ic4[4,2],ic4[4,2],ic5[4,2],ic6[4,2],ic7[4,2],ic8[4,2],ic9[4,2],
                                ic10[4,2],ic11[4,2],ic12[4,2],ic14[4,2],ic14[4,2],ic15[4,2],ic16[4,2],ic17[4,2],ic18[4,2])
)

# los siguientes graficos son para BIM5 pago

ggplot(datos2, aes(x = beta, y = ccz, col = ccz)) +
  geom_point() +
  geom_errorbar(aes(xmin = linf95, xmax = lsup95), width = 0.2) +
  xlab('Estimacion') +
  ylab('CCZ') +
  theme(text = element_text(size = 15)) +
  geom_vline(xintercept=0, linetype='dotted', color='blue', size=1.5)

# y en la escala del odds-ratio
ggplot(datos2, aes(x = or, y = ccz, col = ccz)) +
  geom_point() +
  geom_errorbar(aes(xmin = exp(linf95), xmax = exp(lsup95)), width = 0.2) +
  xlab('OR') +
  ylab('CCZ') +
  theme(text = element_text(size = 15)) +
  geom_vline(xintercept=1, linetype='dotted', color='blue', size=1.5)



umb=0.7 # probabilidad para decir que pago supera este umbral

yhat <- ifelse(predict(mod1_1,type = 'response')>umb,1,0)

table(ccz1$BIM6[!is.na(ccz1$CANT_BI)] , yhat)

length(yhat)
length(ccz1$BIM6)

# hubo que filtrar 17 NA en CANT_BI

table(ccz1$BIM6[!is.na(ccz1$CANT_BI)] , yhat) %>% prop.table(1) #realidad ya sabiendo lo q paso

table(ccz1$BIM6[!is.na(ccz1$CANT_BI)] , yhat) %>% prop.table(2) #para predecir





