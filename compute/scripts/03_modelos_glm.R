#---- LIBRARIES ----

lapply(c("sf", "tidyverse", "tmap", "brms", "writexl", "lubridate", "ggplot2", 
         "caret", "gridExtra", "patchwork", "xtable", "car"), library, character.only = TRUE)

#%%%%%%%%%%%%%%%%%%%%%
#---- FIRST MODEL ----
#%%%%%%%%%%%%%%%%%%%%%

# primer modelo para todos los CCZ

# lectura de TDS totales
dataSP3 <- st_read("compute/data/dataSP3.shp") #1 min en cargar

#table(st_is_valid(dataSP3)) # demora un poco, pero no hay observaciones invalidas

# construyo la variable antiguedad con la variable F_APERTURA
dataSP3 <- dataSP3 %>% 
  mutate(ANTIGUEDAD = as.period(interval(F_APERT, as.Date("2022-12-23")))/years(1))


# transformacion de variables
dataSP3$CATEGOR <- as.factor(dataSP3$CATEGOR)
dataSP3$BIM6 <- as.factor(dataSP3$BIM6) 
dataSP3$BIM5 <- as.factor(dataSP3$BIM5)
dataSP3$BIM4 <- as.factor(dataSP3$BIM4)
dataSP3$BIM3 <- as.factor(dataSP3$BIM3)
dataSP3$BIM2 <- as.factor(dataSP3$BIM2)
dataSP3$BIM1 <- as.factor(dataSP3$BIM1)
dataSP3$TS_CORT <- as.factor(dataSP3$TS_CORT)

dataSP3$CCZ <- as.factor(dataSP3$CCZ)

# relevel permite cambiar la categoria de referencia
dataSP3$BIM1 <- relevel(dataSP3$BIM1, ref = "0")
dataSP3$BIM2 <- relevel(dataSP3$BIM2, ref = "0")
dataSP3$BIM3 <- relevel(dataSP3$BIM3, ref = "0")
dataSP3$BIM4 <- relevel(dataSP3$BIM4, ref = "0")
dataSP3$BIM5 <- relevel(dataSP3$BIM5, ref = "0")
dataSP3$BIM6 <- relevel(dataSP3$BIM6, ref = "0")


# estadistica descriptiva
dataSP3.1 <- dataSP3 %>% as.data.frame() %>% select(-c("CUENTA_", "PADRON", "F_APERT","BIM6", "BIM5", "BIM4", "BIM3", "BIM2", "BIM1", "TS_CORT", "CANT_BI", "geometry")) 
dataSP3.2 <- dataSP3 %>% as.data.frame() %>% select(-c("CUENTA_", "PADRON", "F_APERT","PROP_BI", "ANTIGUEDAD", "CCZ", "CANT_UN", "CATEGOR", "TS_CORT", "CANT_BI", "geometry")) 

descr1 <- summary(dataSP3.1)
descr2 <- summary(dataSP3.2)
################################# volver a hacer todo, tablas, modelos con PROP_BIM_IMP en escala de 0 a 100.
tabla_matrix <- xtable(descr1, caption = "Estadística descripitiva de las variables.")
print(tabla_matrix, type = "latex", include.rownames = FALSE, comment = FALSE, caption.placement='top')
tabla_matrix <- xtable(descr2, caption = "Estadística descripitiva de las variables.")
print(tabla_matrix, type = "latex", include.rownames = FALSE, comment = FALSE, caption.placement='top')



set.seed(1) # para reproducibilidad

# preparamos listas para almacenar variables
ccz <- list()
trainDataList <- list()
testDataList <- list()
modelos <- list()
coeficientes <- list()
intervalos <- list()
predictions <- list()
predictedClass <- list()
confusionMatrix <- list()
aic <- list()

# iteramos sobre las listas de data frames
for(i in 1:18){
  
  print(paste('CCZ:', i))
  
  #create ccz filter from dataSP3
  ccz[[i]] <- dataSP3 %>% filter(CCZ == as.character(i))
  
  #create train and test data
  splitIndex <- createDataPartition(ccz[[i]]$BIM6, p = 0.9, list = FALSE)
  trainDataList[[i]] <- ccz[[i]][splitIndex, ]
  testDataList[[i]] <- ccz[[i]][-splitIndex, ]
  
  #create models
  modelos[[i]] <- glm(BIM6 ~ CANT_UN + CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + ANTIGUEDAD + PROP_BI,
                      data = trainDataList[[i]], family = binomial(link = 'logit'))
  
  # create coeficients and intervals
  coeficientes[[i]] <- coef(modelos[[i]])
  intervalos[[i]] <- confint(modelos[[i]])
  
  #predictions and testing
  predictions[[i]] <- predict(modelos[[i]], newdata = testDataList[[i]], type = "response")
  predictedClass[[i]] <- ifelse(predictions[[i]] > 0.5, 1, 0)
  confusionMatrix[[i]] <- table(Predicted = predictedClass[[i]], Actual = testDataList[[i]]$BIM6)
  print(confusionMatrix[[i]])
  print(paste('Error Global:', 
              (round((confusionMatrix[[i]][1,2] + confusionMatrix[[i]][2,1]) / sum(confusionMatrix[[i]]),3)*100),
              "%"))
  print(paste('Sensibilidad:', 
              (round((confusionMatrix[[i]][2,2]) / (confusionMatrix[[i]][2,2]+confusionMatrix[[i]][1,2]),3)*100),
              "%"))
  print(paste('Especificidad:', 
              (round((confusionMatrix[[i]][1,1]) / (confusionMatrix[[i]][1,1]+confusionMatrix[[i]][2,1]),3)*100),
              "%"))
  print(paste('Precisión:', 
              (round((confusionMatrix[[i]][2,2]) / (confusionMatrix[[i]][2,2]+confusionMatrix[[i]][2,1]),3)*100),
              "%"))
  
  #aic
  aic[[i]] <- modelos[[i]]$aic
  print(paste('AIC:', aic[[i]]))
  
}

# mean of error global
errores <- list()
for(i in 1:18){
  errores[i] <- (round((confusionMatrix[[i]][1,2] + confusionMatrix[[i]][2,1]) / sum(confusionMatrix[[i]]),3)*100)
  }

media_global_error <- mean(unlist(errores))




# for summary models in ccz 1, 5 and 17 
summary(modelos[[1]])

# for printing summary tables in latex 
tabla_latex1 <- xtable(as.data.frame(summary(modelos[[1]])$coefficients), 
                      caption = "Resumen de Modelo", display = c("s","f","f","f","f"), 
                      digits = c(0, 2, 2, 2, 3))
tabla_latex5 <- xtable(as.data.frame(summary(modelos[[5]])$coefficients), 
                       caption = "Resumen de Modelo", display = c("s","f","f","f","f"), 
                       digits = c(0, 2, 2, 2, 3))
tabla_latex17 <- xtable(as.data.frame(summary(modelos[[17]])$coefficients), 
                       caption = "Resumen de Modelo", display = c("s","f","f","f","f"), 
                       digits = c(0, 2, 2, 2, 3))
print(tabla_latex1, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')

print(tabla_latex5, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')

print(tabla_latex17, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')





#dataframe for beta=CATEGORDOMICILIARIO
datos <- data.frame(ccz  = as.factor(1:18),
                    beta = c(coeficientes[[1]][3],coeficientes[[2]][3],coeficientes[[3]][3],coeficientes[[4]][3],coeficientes[[5]][3],coeficientes[[6]][3],coeficientes[[7]][3],coeficientes[[8]][3],coeficientes[[9]][3],coeficientes[[10]][3],
                             coeficientes[[11]][3],coeficientes[[12]][3],coeficientes[[13]][3],coeficientes[[14]][3],coeficientes[[15]][3],coeficientes[[16]][3],coeficientes[[17]][3],coeficientes[[18]][3]),
                    or = exp(c(coeficientes[[1]][3],coeficientes[[2]][3],coeficientes[[3]][3],coeficientes[[4]][3],coeficientes[[5]][3],coeficientes[[6]][3],coeficientes[[7]][3],coeficientes[[8]][3],coeficientes[[9]][3],coeficientes[[10]][3],
                               coeficientes[[11]][3],coeficientes[[12]][3],coeficientes[[13]][3],coeficientes[[14]][3],coeficientes[[15]][3],coeficientes[[16]][3],coeficientes[[17]][3],coeficientes[[18]][3])),
                    linf95 = c(intervalos[[1]][3,1],intervalos[[2]][3,1],intervalos[[3]][3,1],intervalos[[4]][3,1],intervalos[[5]][3,1],intervalos[[6]][3,1],intervalos[[7]][3,1],intervalos[[8]][3,1],intervalos[[9]][3,1],
                               intervalos[[10]][3,1],intervalos[[11]][3,1],intervalos[[12]][3,1],intervalos[[13]][3,1],intervalos[[14]][3,1],intervalos[[15]][3,1],intervalos[[16]][3,1],intervalos[[17]][3,1],intervalos[[18]][3,1]),
                    lsup95 = c(intervalos[[1]][3,2],intervalos[[2]][3,2],intervalos[[3]][3,2],intervalos[[4]][3,2],intervalos[[5]][3,2],intervalos[[6]][3,2],intervalos[[7]][3,2],intervalos[[8]][3,2],intervalos[[9]][3,2],
                               intervalos[[10]][3,2],intervalos[[11]][3,2],intervalos[[12]][3,2],intervalos[[13]][3,2],intervalos[[14]][3,2],intervalos[[15]][3,2],intervalos[[16]][3,2],intervalos[[17]][3,2],intervalos[[18]][3,2])
                    )

datos$color_indicator <- ifelse(datos$linf95 <= 0 & datos$lsup95 >= 0, "No significativo", "Significativo")
datos$color_indicator1 <- ifelse(exp(datos$linf95) <= 1 & exp(datos$lsup95) >= 1, "No significativo", "Significativo")

categ.uno <- ggplot(datos, aes(x = beta, y = ccz, col = color_indicator)) +
  geom_point() +
  geom_errorbar(aes(xmin = linf95, xmax = lsup95), width = 0.2) +
  xlab(expression(beta[2]~"(CATEG.DOM)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos$ccz))) +
  scale_x_continuous(breaks = c(-2,-1.5,-1,-.5,0,0.5,1,1.5,2,2.5)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3), # Ajusta los ticks del eje X para que sean visibles
    axis.ticks.length = unit(0.2, "cm"), # Puedes ajustar el tamaño para hacerlo más visible según tus necesidades
    axis.text.x = element_text(size = 8))

# y en la escala del odds-ratio
categ.dos <- ggplot(datos, aes(x = or, y = ccz, col = color_indicator1)) +
  geom_point() +
  geom_errorbar(aes(xmin = exp(linf95), xmax = exp(lsup95)), width = 0.2) +
  xlab(expression(e^{beta[2]}~"(CATEG.DOM)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos$ccz))) +
  scale_x_continuous(breaks = c(0,0.5,0.75,1,1.25,1.5,1.75,2,2.5,3,4)) +
  coord_cartesian(xlim = c(0, 3)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8))

# plot categ.uno y categ.dos in one row 2 column
combined_plot <- categ.uno + categ.dos + 
  plot_layout(ncol = 2, guides = "collect")

combined_plot
#ggsave("probando.png", plot = combined_plot, units = "px", width = 950)
# no me funciona ggsave, guardar en tamano 950*537 and mantain aspect ratiocant_un_coeficientes



#dataframe for beta=BIM5.1 (pago) es decir bimestre anterior pago.
datos2 <- data.frame(ccz = as.factor(1:18),
                     beta = c(coeficientes[[1]][5],coeficientes[[2]][5],coeficientes[[3]][5],coeficientes[[4]][5],coeficientes[[5]][5],coeficientes[[6]][5],coeficientes[[7]][5],coeficientes[[8]][5],coeficientes[[9]][5],coeficientes[[10]][5],
                              coeficientes[[11]][5],coeficientes[[12]][5],coeficientes[[13]][5],coeficientes[[14]][5],coeficientes[[15]][5],coeficientes[[16]][5],coeficientes[[17]][5],coeficientes[[18]][5]),
                     linf95 = c(intervalos[[1]][5,1],intervalos[[2]][5,1],intervalos[[3]][5,1],intervalos[[4]][5,1],intervalos[[5]][5,1],intervalos[[6]][5,1],intervalos[[7]][5,1],intervalos[[8]][5,1],intervalos[[9]][5,1],
                                intervalos[[10]][5,1],intervalos[[11]][5,1],intervalos[[12]][5,1],intervalos[[13]][5,1],intervalos[[14]][5,1],intervalos[[15]][5,1],intervalos[[16]][5,1],intervalos[[17]][5,1],intervalos[[18]][5,1]),
                     lsup95 = c(intervalos[[1]][5,2],intervalos[[2]][5,2],intervalos[[3]][5,2],intervalos[[4]][5,2],intervalos[[5]][5,2],intervalos[[6]][5,2],intervalos[[7]][5,2],intervalos[[8]][5,2],intervalos[[9]][5,2],
                                intervalos[[10]][5,2],intervalos[[11]][5,2],intervalos[[12]][5,2],intervalos[[13]][5,2],intervalos[[14]][5,2],intervalos[[15]][5,2],intervalos[[16]][5,2],intervalos[[17]][5,2],intervalos[[18]][5,2])
)

datos2$color_indicator <- ifelse(datos2$linf95 <= 0 & datos2$lsup95 >= 0, "No significativo", "Significativo")
datos2$color_indicator1 <- ifelse(exp(datos2$linf95) <= 1 & exp(datos2$lsup95) >= 1, "No significativo", "Significativo")

bim5.uno <- ggplot(datos2, aes(x = beta, y = ccz, col = color_indicator)) +
  geom_point() +
  geom_errorbar(aes(xmin = linf95, xmax = lsup95), width = 0.2) +
  xlab(expression(beta[4]~"(BIM5.1)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos2$ccz))) +  # Asegúrate de que 'datos2' sea el dataframe correcto
  scale_x_continuous(breaks = c(0,3,4,5,6,7,8,12)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8))


# y en la escala del odds-ratio
bim5.dos <- ggplot(datos2, aes(x = exp(beta), y = ccz, col = color_indicator1)) +
  geom_point() +
  geom_errorbar(aes(xmin = exp(linf95), xmax = exp(lsup95)), width = 0.2) +
  xlab(expression(e^{beta[4]}~"(BIM5.1)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos$ccz))) +
  scale_x_continuous(breaks = c(1,50,100,150,200,250,300,350,400,500,600,700,800)) +
  coord_cartesian(xlim = c(0, 600)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8))

# plot categ.uno y categ.dos in one row 2 column
combined_plot2 <- bim5.uno + bim5.dos + 
  plot_layout(ncol = 2, guides = "collect")

combined_plot2
# guardar en tamano 950*537 and mantain aspect ratio. Name: bim5.1_coeficientes


#dataframe for beta=CANT_UN
datos3 <- data.frame(ccz = as.factor(1:18),
                     beta = c(coeficientes[[1]][2],coeficientes[[2]][2],coeficientes[[3]][2],coeficientes[[4]][2],coeficientes[[5]][2],coeficientes[[6]][2],coeficientes[[7]][2],coeficientes[[8]][2],coeficientes[[9]][2],coeficientes[[10]][2],
                              coeficientes[[11]][2],coeficientes[[12]][2],coeficientes[[13]][2],coeficientes[[14]][2],coeficientes[[15]][2],coeficientes[[16]][2],coeficientes[[17]][2],coeficientes[[18]][2]),
                     linf95 = c(intervalos[[1]][2,1],intervalos[[2]][2,1],intervalos[[3]][2,1],intervalos[[4]][2,1],intervalos[[5]][2,1],intervalos[[6]][2,1],intervalos[[7]][2,1],intervalos[[8]][2,1],intervalos[[9]][2,1],
                                intervalos[[10]][2,1],intervalos[[11]][2,1],intervalos[[12]][2,1],intervalos[[13]][2,1],intervalos[[14]][2,1],intervalos[[15]][2,1],intervalos[[16]][2,1],intervalos[[17]][2,1],intervalos[[18]][2,1]),
                     lsup95 = c(intervalos[[1]][2,2],intervalos[[2]][2,2],intervalos[[3]][2,2],intervalos[[4]][2,2],intervalos[[5]][2,2],intervalos[[6]][2,2],intervalos[[7]][2,2],intervalos[[8]][2,2],intervalos[[9]][2,2],intervalos[[10]][2,2],
                                intervalos[[11]][2,2],intervalos[[12]][2,2],intervalos[[13]][2,2],intervalos[[14]][2,2],intervalos[[15]][2,2],intervalos[[16]][2,2],intervalos[[17]][2,2],intervalos[[18]][2,2]))

datos3$color_indicator <- ifelse(datos3$linf95 <= 0 & datos3$lsup95 >= 0, "No significativo", "Significativo")
datos3$color_indicator1 <- ifelse(exp(datos3$linf95) <= 1 & exp(datos3$lsup95) >= 1, "No significativo", "Significativo")


cant_un.uno <- ggplot(datos3, aes(x = beta, y = ccz, col = color_indicator)) +
  geom_point() +
  geom_errorbar(aes(xmin = linf95, xmax = lsup95), width = 0.2) +
  xlab(expression(beta[1]~"(CANT_UN)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos$ccz))) +
  scale_x_continuous(breaks = c(-0.2,-0.1,0,.1,.2,.3,.4,.5,1,2)) +
  coord_cartesian(xlim = c(-.1, 0.3)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3), 
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8))

# y en la escala del odds-ratio
cant_un.dos <- ggplot(datos3, aes(x = exp(beta), y = ccz, col = color_indicator1)) +
  geom_point() +
  geom_errorbar(aes(xmin = exp(linf95), xmax = exp(lsup95)), width = 0.2) +
  xlab(expression(e^{beta[1]}~"(CANT_UN)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos$ccz))) +
  scale_x_continuous(breaks = c(0.5,0.9,1,1.1,1.2,1.3,1.5,1.8,2,2.5,3,4)) +
  coord_cartesian(xlim = c(0.9, 1.35)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8))

# plot categ.uno y categ.dos in one row 2 column
combined_plot3 <- cant_un.uno + cant_un.dos + 
  plot_layout(ncol = 2, guides = "collect")

combined_plot3
# guardar en tamano 950*537 and mantain aspect ratio. Name: cant_un_coeficientes



#dataframe for beta=PROP_BI
datos4 <- data.frame(ccz = as.factor(1:18),
                     beta = c(coeficientes[[1]][15],coeficientes[[2]][15],coeficientes[[3]][15],coeficientes[[4]][15],coeficientes[[5]][15],coeficientes[[6]][15],coeficientes[[7]][15],coeficientes[[8]][15],coeficientes[[9]][15],
                              coeficientes[[10]][15],coeficientes[[11]][15],coeficientes[[12]][15],coeficientes[[13]][15],coeficientes[[14]][15],coeficientes[[15]][15],coeficientes[[16]][15],coeficientes[[17]][15],coeficientes[[18]][15]),
                     linf95 = c(intervalos[[1]][15,1],intervalos[[2]][15,1],intervalos[[3]][15,1],intervalos[[4]][15,1],intervalos[[5]][15,1],intervalos[[6]][15,1],intervalos[[7]][15,1],intervalos[[8]][15,1],intervalos[[9]][15,1],
                                intervalos[[10]][15,1],intervalos[[11]][15,1],intervalos[[12]][15,1],intervalos[[13]][15,1],intervalos[[14]][15,1],intervalos[[15]][15,1],intervalos[[16]][15,1],intervalos[[17]][15,1],intervalos[[18]][15,1]),
                     lsup95 = c(intervalos[[1]][15,2],intervalos[[2]][15,2],intervalos[[3]][15,2],intervalos[[4]][15,2],intervalos[[5]][15,2],intervalos[[6]][15,2],intervalos[[7]][15,2],intervalos[[8]][15,2],intervalos[[9]][15,2],
                                intervalos[[10]][15,2],intervalos[[11]][15,2],intervalos[[12]][15,2],intervalos[[13]][15,2],intervalos[[14]][15,2],intervalos[[15]][15,2],intervalos[[16]][15,2],intervalos[[17]][15,2],intervalos[[18]][15,2]))

datos4$color_indicator <- ifelse(datos4$linf95 <= 0 & datos4$lsup95 >= 0, "No significativo", "Significativo")
datos4$color_indicator1 <- ifelse(exp(datos4$linf95) <= 1 & exp(datos4$lsup95) >= 1, "No significativo", "Significativo")

prop_bi.uno <- ggplot(datos4, aes(x = beta, y = ccz, col = color_indicator)) +
  geom_point() +
  geom_errorbar(aes(xmin = linf95, xmax = lsup95), width = 0.2) +
  xlab(expression(beta[14]~"(PROP_BI)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos$ccz))) +
  scale_x_continuous(breaks = c(-0.05,-0.04,-0.03,-0.02,-0.01,0.0,0.01)) +
  coord_cartesian(xlim = c(-0.051, 0.01)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3), 
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8))

# y en la escala del odds-ratio
prop_bi.dos <- ggplot(datos4, aes(x = exp(beta), y = ccz, col = color_indicator1)) +
  geom_point() +
  geom_errorbar(aes(xmin = exp(linf95), xmax = exp(lsup95)), width = 0.2) +
  xlab(expression(e^{beta[14]}~"(PROP_BI)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos$ccz))) +
  scale_x_continuous(breaks = c(0.95,0.96,0.97,0.98,0.99,1,1.01,1.02)) +
  coord_cartesian(xlim = c(0.94, 1.02)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8))

# plot categ.uno y categ.dos in one row 2 column
combined_plot4 <- prop_bi.uno + prop_bi.dos + 
  plot_layout(ncol = 2, guides = "collect")

combined_plot4
# guardar en tamano 950*537 and mantain aspect ratio. Name: prop_bim_coeficientes



# dataframe para la variable beta=BIM1.1 en posicion 13
datos5 <- data.frame(ccz = as.factor(1:18),
                     beta = c(coeficientes[[1]][13],coeficientes[[2]][13],coeficientes[[3]][13],coeficientes[[4]][13],coeficientes[[5]][13],coeficientes[[6]][13],coeficientes[[7]][13],coeficientes[[8]][13],coeficientes[[9]][13],
                              coeficientes[[10]][13],coeficientes[[11]][13],coeficientes[[12]][13],coeficientes[[13]][13],coeficientes[[14]][13],coeficientes[[15]][13],coeficientes[[16]][13],coeficientes[[17]][13],coeficientes[[18]][13]),
                     linf95 = c(intervalos[[1]][13,1],intervalos[[2]][13,1],intervalos[[3]][13,1],intervalos[[4]][13,1],intervalos[[5]][13,1],intervalos[[6]][13,1],intervalos[[7]][13,1],intervalos[[8]][13,1],intervalos[[9]][13,1],
                                intervalos[[10]][13,1],intervalos[[11]][13,1],intervalos[[12]][13,1],intervalos[[13]][13,1],intervalos[[14]][13,1],intervalos[[15]][13,1],intervalos[[16]][13,1],intervalos[[17]][13,1],intervalos[[18]][13,1]),
                     lsup95 = c(intervalos[[1]][13,2],intervalos[[2]][13,2],intervalos[[3]][13,2],intervalos[[4]][13,2],intervalos[[5]][13,2],intervalos[[6]][13,2],intervalos[[7]][13,2],intervalos[[8]][13,2],intervalos[[9]][13,2],
                                intervalos[[10]][13,2],intervalos[[11]][13,2],intervalos[[12]][13,2],intervalos[[13]][13,2],intervalos[[14]][13,2],intervalos[[15]][13,2],intervalos[[16]][13,2],intervalos[[17]][13,2],intervalos[[18]][13,2]))

# agregar color indicator
datos5$color_indicator <- ifelse(datos5$linf95 <= 0 & datos5$lsup95 >= 0, "No significativo", "Significativo")
datos5$color_indicator1 <- ifelse(exp(datos5$linf95) <= 1 & exp(datos5$lsup95) >= 1, "No significativo", "Significativo")

bim1.uno <- ggplot(datos5, aes(x = beta, y = ccz, col = color_indicator)) +
  geom_point() +
  geom_errorbar(aes(xmin = linf95, xmax = lsup95), width = 0.2) +
  xlab(expression(beta[12]~"(BIM1.1)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos$ccz))) +
  scale_x_continuous(breaks = c(-3,-2,-1,-.5,0,.5,1,1.5)) +
  coord_cartesian(xlim = c(-3.1, 1.5)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3), 
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8))

# en odds ratio
bim1.dos <- ggplot(datos5, aes(x = exp(beta), y = ccz, col = color_indicator1)) +
  geom_point() +
  geom_errorbar(aes(xmin = exp(linf95), xmax = exp(lsup95)), width = 0.2) +
  xlab(expression(e^{beta[12]}~"(BIM1.1)")) +
  ylab('CCZ') +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = 'dashed', color = 'black', linewidth = 0.1) +
  scale_color_manual(values = c("No significativo" = "grey", "Significativo" = "dodgerblue"),
                     name = "Coeficiente") +
  scale_y_discrete(limits = rev(levels(datos$ccz))) +
  scale_x_continuous(breaks = c(0,.5,1,1.5,2,3,4)) +
  coord_cartesian(xlim = c(0, 4)) +
  theme(
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text.x = element_text(size = 8))

# plot bim1.uno y bim1.dos in one row 2 column
combined_plot5 <- bim1.uno + bim1.dos + 
  plot_layout(ncol = 2, guides = "collect")

combined_plot5
# guardar en tamano 950*537 and mantain aspect ratio. Name: bim1.1_coeficientes




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---- BIM 5 determina el BIM6 ? ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# matriz de confusion de BIM6 contra BIM5 sin los valores -1 en dataSP3
t <- table(dataSP3$BIM6, dataSP3$BIM5)

t <- t[,-2]

t2 <- prop.table(t,2) # proporciones

t[1,1]/(t[1,1]+t[2,1]) # probabilidad de pagar el BIM6 si pago el BIM5
t[2,2]/(t[1,2]+t[2,2]) # probabilidad de no pagar el BIM6 si no pago el BIM5

( t[1,1]/(t[1,1]+t[2,1]) + t[2,2]/(t[1,2]+t[2,2]) ) / 2

# consultar si esta correcto decir que se predice con 94% de eficiencia si solo incluye BIM5

1 - (1840+18333)/(nrow(dataSP3)) 

# matriz de confusion de BIM6 contra BIM5
tabla_matrix <- xtable(t, caption = "Resumen de Modelo")
print(tabla_matrix, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')


# filtrar las que pagaron siempre y por otro lado los que no pagaron nunca, mostrar
# que las que pagan casi siempre pagan siempre, y las que no pagan casi nunca pagan,
# de esta forma se justifica que dichas observaciones no aportan informacion para el modelo
pagaron_nunca <- dataSP3 %>% filter(BIM5 == 0, BIM4 == 0, BIM3 == 0, BIM2 ==0, BIM1 == 0)
table(pagaron_nunca$BIM6)[2]/nrow(pagaron_nunca)

pagaron_siempre <- dataSP3 %>% filter(BIM5 == 1, BIM4 == 1, BIM3 == 1, BIM2 == 1, BIM1 == 1)
table(pagaron_siempre$BIM6)[1]/nrow(pagaron_siempre)


# observacione que el BIM5 no pagaron
no_pagaron <- dataSP3 %>% filter(BIM5 == 0)
1- table(no_pagaron$BIM6)[2] / nrow(no_pagaron) # probabilidad de pagar el BIM6 si no pagaron el BIM5

# observaciones que el BIM5 pagaron
pagaron <- dataSP3 %>% filter(BIM5 == 1)
1 - table(pagaron$BIM6)[1] / nrow(pagaron) # probabilidad de no pagar el BIM6 si pagaron el BIM5



# cual es la distribucion de observaciones de pagaron_nunca segun ccz
table(pagaron_nunca$CCZ)

# me lo saltee
#---- SECOND MODEL ----

# justifica porque se descide filtrar observaciones

# observaciones que a veces pagaron y a veces no en los bimestres anteriores
dataSP3.2 <- dataSP3 %>%
  filter(!(BIM5 == 1 & BIM4 == 1 & BIM3 == 1 & BIM2 == 1 & BIM1 == 1 | 
           BIM5 == 0 & BIM4 == 0 & BIM3 == 0 & BIM2 == 0 & BIM1 == 0 ) )

table(dataSP3.2$BIM6)[2] / nrow(dataSP3.2) # probabilidad de no pagar el BIM6 si pagaron el BIM5

# se rehacen los modelos para dataSP3.2

ccz <- list()
trainDataList <- list()
testDataList <- list()
modelos <- list()
coeficientes <- list()
intervalos <- list()
predictions <- list()
predictedClass <- list()
confusionMatrix <- list()
aic <- list()

# iteramos sobre las listas de data frames
for(i in 1:18){
  
  print(paste('CCZ:', i))
  
  #create ccz filter from dataSP3
  ccz[[i]] <- dataSP3.2 %>% filter(CCZ == as.character(i))
  
  #create train and test data
  splitIndex <- createDataPartition(ccz[[i]]$BIM6, p = 0.7, list = FALSE)
  trainDataList[[i]] <- ccz[[i]][splitIndex, ]
  testDataList[[i]] <- ccz[[i]][-splitIndex, ]
  
  #create models
  modelos[[i]] <- glm(BIM6 ~ CANT_UN + CATEGOR + BIM5 + BIM4 + BIM3 + BIM2 + BIM1 + ANTIGUEDAD + CANT_BI + PROP_BI,
                      data = trainDataList[[i]], family = binomial(link = 'logit'))
  
  # create coeficients and intervals
  coeficientes[[i]] <- coef(modelos[[i]])
  intervalos[[i]] <- confint(modelos[[i]])
  
  #predictions and testing
  predictions[[i]] <- predict(modelos[[i]], newdata = testDataList[[i]], type = "response")
  predictedClass[[i]] <- ifelse(predictions[[i]] > 0.5, 1, 0)
  confusionMatrix[[i]] <- table(Predicted = predictedClass[[i]], Actual = testDataList[[i]]$BIM6)
  print(confusionMatrix[[i]])
  print(paste('Error Global:', 
              (round((confusionMatrix[[i]][1,2] + confusionMatrix[[i]][2,1]) / sum(confusionMatrix[[i]]),3)*100),
              "%"))
  
  #aic
  aic[[i]] <- modelos[[i]]$aic
  
}

# for summary models in ccz 1, 5 and 17 
summary(modelos[[1]])

# y se vuelve a observar que en todos los modelos la chance de pagar el BIM6 si 
#se pago el BIM5 es de alrededor a 130, es decir 130 veces mas probable de pagar

# sigue pasando lo mismo considerando solo BIM5, entonces pareciera que sigue determinando al BIM6
# el modelo en sintensis no esta aportando mucho
prop.table(table(dataSP3.2$BIM6, dataSP3.2$BIM5)[,-2],1)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---- THIRD MODEL ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# entonces se decide construir el modelo sin variables de BIMESTRES anteriores

# observaciones que tienen todas -1 en BIM5, BIM4, BIM3, BIM2 y BIM1
nunca_generaron <- dataSP3 %>% filter(BIM5 == -1, BIM4 == -1, BIM3 == -1, 
                                      BIM2 == -1, BIM1 == -1)

# que cantidad de las restantes pagaron o no pagaron el BIM6
table(nunca_generaron$BIM6)


# observaciones que el BIM5 tienen deuda sin generar
sin_generar <- dataSP3 %>% filter(BIM5 == -1) 

table(sin_generar$BIM6)

# estadistica descriptiva de las variables en sin_generar
sin_generar2 <- sin_generar %>% as.data.frame() %>% select(-c("CUENTA_", "PADRON", "F_APERT", "BIM5", "BIM4", "BIM3", "BIM2", "BIM1", "TS_CORT", "CANT_BI", "geometry")) 

descr <- summary(sin_generar2)
tabla_matrix <- xtable(descr, caption = "Estadística descripitiva de las variables.", display = c("f","f","f","f","f","f","f"), 
                       digits = c(0, 0, 0, 0, 0, 3, 3))
print(tabla_matrix, type = "latex", include.rownames = FALSE, comment = FALSE, caption.placement='top')

  
#create train and test data
splitIndex <- createDataPartition(sin_generar$BIM6, p = 0.9, list = FALSE)
trainDataList <- sin_generar[splitIndex, ]
testDataList <- sin_generar[-splitIndex, ]

#create models
modelos <- glm(BIM6 ~ CCZ + CANT_UN + CATEGOR + ANTIGUEDAD + PROP_BI,
               data = trainDataList, family = binomial(link = 'logit'))

# create coeficients and intervals
coeficientes <- coef(modelos)
intervalos <- confint(modelos)

#predictions and testing
predictions <- predict(modelos, newdata = testDataList, type = "response")
predictedClass <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix <- table(Predicted = predictedClass, Actual = testDataList$BIM6)
print(confusionMatrix)
print(paste('Error Global:', 
            (round((confusionMatrix[1,2] + confusionMatrix[2,1]) / sum(confusionMatrix),3)*100),
            "%"))
print(paste('Sensibilidad:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[1,2]),3)*100),
            "%"))
print(paste('Especificidad:', 
            (round((confusionMatrix[1,1]) / (confusionMatrix[1,1]+confusionMatrix[2,1]),3)*100),
            "%"))
print(paste('Precisión:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[2,1]),3)*100),
            "%"))
# la variable CCZ no es significativa, se prueba sin ella
# sin la variable CCZ la signifcancia de BIM4 toma el rol de BIM5

# for summary models in ccz 1, 5 and 17 
summary(modelos)

tabla_latex <- xtable(as.data.frame(summary(modelos)$coefficients), 
                      caption = "Resumen de Modelo", display = c("s","f","f","f","f"), 
                      digits = c(0, 2, 2, 2, 3))
print(tabla_latex, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')


Anova(modelos)

significacion_conjunta <- xtable(as.data.frame(Anova(modelos)), 
                                 caption = "Tests de Significación Conjunta del Modelo", display = c("s","f","f","f"), 
                                 digits = c(0, 2, 0, 3))
print(significacion_conjunta, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')

vif(modelos)


# iteraciones para obtener el error global, sensibilidad, especificidad y precisión

errores <- c()
sensibilidad <- c()
especificidad <- c()
precision <- c()

for(i in 1:500){
  
  print(paste("Iteración",i))
  #create train and test data
  splitIndex <- createDataPartition(sin_generar$BIM6, p = 0.9, list = FALSE)
  trainDataList <- sin_generar[splitIndex, ]
  testDataList <- sin_generar[-splitIndex, ]
  
  #create models
  modelos <- glm(BIM6 ~ CCZ + CANT_UN + CATEGOR + ANTIGUEDAD + PROP_BI,
                 data = trainDataList, family = binomial(link = 'logit'))
  
  #predictions and testing
  predictions <- predict(modelos, newdata = testDataList, type = "response")
  predictedClass <- ifelse(predictions > 0.5, 1, 0)
  confusionMatrix <- table(Predicted = predictedClass, Actual = testDataList$BIM6)
  
  errores[i] <- (round((confusionMatrix[1,2] + confusionMatrix[2,1]) / sum(confusionMatrix),3)*100)
  sensibilidad[i] <- (round((confusionMatrix[2,2]) / (confusionMatrix[2,2] + confusionMatrix[2,1]),3)*100)
  especificidad[i] <- (round((confusionMatrix[1,1]) / (confusionMatrix[1,1] + confusionMatrix[1,2]),3)*100)
  precision[i] <- (round((confusionMatrix[2,2]) / (confusionMatrix[2,2] + confusionMatrix[1,2]),3)*100)
  
}

mean(errores) # 19.09
mean(sensibilidad) # 77.95
mean(especificidad) # 85.89
mean(precision) # 88.86

# descartamos coeficientes no significativos




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---- FOURTH MODEL ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#create train and test data
splitIndex <- createDataPartition(sin_generar$BIM6, p = 0.9, list = FALSE)
trainDataList <- sin_generar[splitIndex, ]
testDataList <- sin_generar[-splitIndex, ]

#create models
modelos_1 <- glm(BIM6 ~  ANTIGUEDAD + PROP_BI,
               data = trainDataList, family = binomial(link = 'logit'))

# create coeficients and intervals
coeficientes <- coef(modelos_1)
intervalos <- confint(modelos_1)

#predictions and testing
predictions <- predict(modelos_1, newdata = trainDataList, type = "response")
predictedClass <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix <- table(Predicted = predictedClass, Actual = trainDataList$BIM6)
print(confusionMatrix)
print(paste('Error Global:', 
            (round((confusionMatrix[1,2] + confusionMatrix[2,1]) / sum(confusionMatrix),3)*100),
            "%"))
print(paste('Sensibilidad:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[1,2]),3)*100),
            "%"))
print(paste('Especificidad:', 
            (round((confusionMatrix[1,1]) / (confusionMatrix[1,1]+confusionMatrix[2,1]),3)*100),
            "%"))
print(paste('Precisión:', 
            (round((confusionMatrix[2,2]) / (confusionMatrix[2,2]+confusionMatrix[2,1]),3)*100),
            "%"))


summary(modelos_1)

tabla_latex2 <- xtable(as.data.frame(summary(modelos_1)$coefficients), 
                      caption = "Resumen de Modelo", display = c("s","f","f","f","f"), 
                      digits = c(0, 2, 2, 2, 3))
print(tabla_latex2, type = "latex", include.rownames = TRUE, comment = FALSE, caption.placement='top')

# iteraciones para obtener el error global, sensibilidad, especificidad y precisión

errores <- c()
sensibilidad <- c()
especificidad <- c()
precision <- c()

for(i in 1:1000){
  
  #print(paste("Iteración",i))
  #create train and test data
  splitIndex <- createDataPartition(sin_generar$BIM6, p = 0.9, list = FALSE)
  trainDataList <- sin_generar[splitIndex, ]
  testDataList <- sin_generar[-splitIndex, ]
  
  #create models
  modelos_1 <- glm(BIM6 ~  ANTIGUEDAD + PROP_BI,
                   data = trainDataList, family = binomial(link = 'logit'))
  
  #predictions and testing
  predictions <- predict(modelos_1, newdata = trainDataList, type = "response")
  predictedClass <- ifelse(predictions > 0.5, 1, 0)
  confusionMatrix <- table(Predicted = predictedClass, Actual = trainDataList$BIM6)
  
  errores[i] <- (round((confusionMatrix[1,2] + confusionMatrix[2,1]) / sum(confusionMatrix),5)*100)
  sensibilidad[i] <- (round((confusionMatrix[2,2]) / (confusionMatrix[2,2] + confusionMatrix[2,1]),3)*100)
  especificidad[i] <- (round((confusionMatrix[1,1]) / (confusionMatrix[1,1] + confusionMatrix[1,2]),3)*100)
  precision[i] <- (round((confusionMatrix[2,2]) / (confusionMatrix[2,2] + confusionMatrix[1,2]),3)*100)
  
}

mean(errores) # 18.28
mean(sensibilidad) # 75.06
mean(especificidad) # 95.39
mean(precision) # 97.10



#%%%%%%%%%%%%%%%%%%%%%
#--------PLOTS -------
#%%%%%%%%%%%%%%%%%%%%%
# plot with tmap las obs en sin_generar coloreadas por BIM6
tmap_mode("view")
tm_shape(sin_generar) +
  tm_dots(col = "BIM6", size = 0.1, shape = 16, palette = "viridis") +
  tm_view(view.legend.position = c("right", "top")) +
  tmap_options(check.and.fix = TRUE)

#1180*500 mantain aspect ratio

# observacion del barrio san nicolas, bimestres anteriores sin generar, porque la tarifa es nueva
sin_generar[sin_generar$PADRON==431519,]


#%%%%%%%%%%%%%%%%%%%%%
#---- observesen las variables PROP_BIM_IMP y BIM6 promedidas a nivel de CCZ ----

# cargamos shape de ccz
ccz <- st_read('compute/data/ine_ccz_mvd.shp')
# y le asigno el crs de los carlitos
st_crs(ccz)<- st_crs(sin_generar)


#%%%%%%%%%%%%%%%%%%%%%
# creamos variable media de la variable BIM6 agrupado por ccz para todas las obs.
media_bim6 <- dataSP3 %>% select(CCZ,BIM6) %>%
  group_by(CCZ) %>% 
  summarise(media = mean(as.numeric(BIM6))-1) %>% as.data.frame()

media_bim6[order(media_bim6$media),]
media_bim6 <- media_bim6[,-3]
media_bim6$CCZ <- as.integer(media_bim6$CCZ)
ccz <- left_join(ccz,media_bim6,by=c("CCZ"="CCZ"))

# plot ccz coloreada por media_BIM6
tmap_mode("view")
tm_shape(ccz)+
  tm_polygons(col="media",title="",border.col="red",alpha =.5, breaks=c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_text("CCZ", size = 1.5)



#%%%%%%%%%%%%%%%%%%%%%
# creamos variable, media de la variable BIM6 agrupado por ccz pero ahora para el conjunto sin_generar de observaciones
media_bim6 <- sin_generar %>% select(CCZ,BIM6) %>%
  group_by(CCZ) %>% 
  summarise(media = mean(as.numeric(BIM6))-1) %>% as.data.frame()

media_bim6 <- media_bim6[,-3]
media_bim6$CCZ <- as.integer(media_bim6$CCZ)
ccz2 <- left_join(ccz,media_bim6,by=c("CCZ"="CCZ"))

# plot ccz2 coloreada por media_BIM6
tmap_mode("view")
tm_shape(ccz2)+
  tm_polygons(col="media",title="",border.col="red",alpha =.5, breaks=c(0.3, 0.4, 0.5, 0.55, 0.6, 0.65, 0.7, 0.8, 0.9))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_text("CCZ", size = 1.5)



#%%%%%%%%%%%%%%%%%%%%%
# ahora observamos la media de prop_bim_imp por ccz de todas las obs.
media_prop_bi <- dataSP3 %>% select(CCZ,PROP_BI) %>%
  group_by(CCZ) %>% 
  summarise(media = mean(as.numeric(PROP_BI))*100) %>% as.data.frame()

media_prop_bi[order(media_prop_bi$media),]
media_prop_bi <- media_prop_bi[,-3]
media_prop_bi$CCZ <- as.integer(media_prop_bi$CCZ)
ccz3 <- left_join(ccz,media_prop_bi,by=c("CCZ"="CCZ"))

# plot ccz coloreada por media_PROP_BI
tmap_mode("view")
tm_shape(ccz3)+
  tm_polygons(col="media",title="",border.col="red",alpha =.5, breaks=c())+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_text("CCZ", size = 1.5)



# a partir de dataSP3 seleccionar la variabla PROP_BI y promediarla en total
mean(as.numeric(dataSP3$PROP_BI))*100
