#---- LIBRARIES ----

lapply(c("readxl", "tidyverse", "sf", "writexl", "tmap"),
       library, character.only = TRUE)

#---- CARGAMOS LOS DATOS DE LAS TARIFAS ----
# Lectura y procesamiento de los datos 'SD_118827_2'

data <- read.csv('compute/data/SD_118827_2.csv', header = TRUE, sep = ";", dec = ",")

#head(data)
#str(data)
#summary(data)


#---- VARIABLES ----

data$F_APERTURA <- as.Date(data$F_APERTURA, format = "%d/%m/%Y")
data <- data %>% mutate(ANTIGUEDAD = as.period(interval(F_APERT, as.Date("2022-12-23")))/years(1))

#str(data$F_APERTURA)

data$CANT_UNID <- as.numeric(data$CANT_UNID)

data$CATEGORIA <- as.factor(data$CATEGORIA)

#str(data$BIM6)
#levels(data$BIM6)

data$BIM6 <- as.factor(data$BIM6)
#levels(data$BIM6) <- list(impago='0', sin_generar='-1',pago='1')

data$BIM5 <- as.factor(data$BIM5)
#levels(data$BIM5) <- list(impago='0', sin_generar='-1',pago='1')

data$BIM4 <- as.factor(data$BIM4)
#levels(data$BIM4) <- list(impago='0', sin_generar='-1',pago='1')

data$BIM3 <- as.factor(data$BIM3)
#levels(data$BIM3) <- list(impago='0', sin_generar='-1',pago='1')

data$BIM2 <- as.factor(data$BIM2)
#levels(data$BIM2) <- list(impago='0', sin_generar='-1',pago='1')

data$BIM1 <- as.factor(data$BIM1)
#levels(data$BIM1) <- list(impago='0', sin_generar='-1',pago='1')

#levels.bim <- c('sin generar','pago','impago')

data$TS_CORTADA <- as.factor(data$TS_CORTADA)
#levels(data$TS_CORTADA) <- list(activa='0',cortada='1')


#---- NUEVAS VARIABLES ----

# las cuentas activas ya tienen generadas las deudas del proximo bimestre, por tal razon 
# todas las cuentas que estan al dia, figuran con un bimestre impago, dicho bimestre es el
# de diciembre-enero que corresponde pagar hasta el 01/02/23.
# Por lo tanto a dichas cuentas les restamos 1 bim impago, porque corresponde a la deuda 
# generada del proximo bimestre que aun no estaba en condiciones de ser paga.

data$CANT_BIM_IMPAGOS <- as.numeric(data$CANT_BIM_IMPAGOS)
data <- data %>% mutate(CANT_BIM_IMPAGOS = ifelse(TS_CORTADA == '0' &
                                                    CANT_BIM_IMPAGOS >= 1, CANT_BIM_IMPAGOS - 1, CANT_BIM_IMPAGOS))


# creamos una nueva variable porcentaje de bimestres impagos que integra la cantidad 
# de bimestres impagos con la antiguedad

data <- data  %>% mutate(PROP_BIM_IMPAGOS = (CANT_BIM_IMPAGOS / (as.numeric(ANTIGUEDAD)*6))*100)

# hay algunas cuentas (TDS) con proporcion mayor a 1 yo creo que esto se debe a que
# algunas de las 2 variables involucradas se encuentra mal calculada para algunas
# observaciones, por lo tanto la solucion es redondear dichas obs. a 1

data$PROP_BIM_IMPAGOS <- ifelse(data$PROP_BIM_IMPAGOS > 100, 1, data$PROP_BIM_IMPAGOS)

#summary(data)
#str(data)


#---- CARGA Y PROCESAMIENTO DE DATOS ESPACIALES ----

# asignamos la geometria espacial por padron a cada cuenta TDS

padrones <- st_read("compute/data/v_mdg_parcelas.shp")
anteriores <- st_read("compute/data/v_mdg_parcelas_historico.shp")
ccz <- st_read("compute/data/sig_comunales.shp")

#table(is.na(data$PADRON)) #no hay valores faltantes

#filtramos las cuentas que no tienen padron asignado
data2 <- data %>% filter(PADRON!='0') 
#100 cuentas filtradas que son ESPACIO PUBLICO

#table(duplicated(data$CUENTA_TS)) #sin duplicados por cuenta
#table(duplicated(data2$PADRON)) #121336 duplicados , ejemplo padron 2482 esta duplicado, 
                                 #esto es porque hay varias cuentas en dicho padron

data2 <- data2[!duplicated(data2$PADRON), ]


#unimos los padrones actuales con los anteriores
anteriores2 <- anteriores %>% select(c(PADRON,geometry)) %>% filter(PADRON%in%data2$PADRON) #son 336

#filtro los padrones con TDS
padrones2 <- padrones %>% select(c(PADRON,geometry)) %>% filter(PADRON%in%data2$PADRON) #son 170674 padrones distintos

#length(unique(padrones2$PADRON)) #170651 padrones unicos

#juntamos, 170674 + 336 = 171010
padrones3 <- rbind(padrones2,anteriores2)

#length(unique(padrones3$PADRON)) #170899
#which(duplicated(padrones3$PADRON)) #hay 55 padrones unicos duplicados


#se observan padrones repetidos, esto es porque tienen varios poligonos
obs <- padrones3[duplicated(padrones3$PADRON),] 

#write_xlsx(obs,"copmute/data/duplic.xlsx") 

#son 55 padrones con geometrias varias, solucion unirlas en unico polygon
#seleccionamos los padrones que estan en nuestros datos y seleccionamos todos sus poligonos para unirlos en 1 
polygons_varios <- padrones3[(padrones3$PADRON%in%obs$PADRON),] 

polygons_union <- polygons_varios %>% group_by(PADRON) %>% summarize(geometry = st_union(geometry)) 
#quedaron 55

padrones_sin_duplic <- padrones3[!(padrones3$PADRON%in%obs$PADRON),] 
#quedan 170844, mas los polygon_union (55) = 170899

#volvemos a unir
padrones4 <- rbind(padrones_sin_duplic,polygons_union) #quedan 170899 (deberian ser 170991 porque data2 contiene 170991 padrones distintos)

#length(unique(padrones4$PADRON)) #170899 (mismo length)
#which(duplicated(padrones4$PADRON)) #0

#busquemos los padrones que no estan en data (170991, 92 padrones faltantes) 
padrones_faltantes <- data2[!(data2$PADRON%in%padrones4$PADRON),]
#estos 92 padrones sufrieron modificaciones prediales ya sean fusiones, etc y no figuran en la capa de padrones anteriores (faltan alli), todos tienen padrones nuevos.
#solucion es excluirlos y tomarlos como NA's


#filtro 100 cuentas en ESPACIO PUBLICO fueron filtradas por no tener padron asignado, quedan 292327
data3 <- data %>% filter(PADRON!='0') 
#son 170899 padrones unicos, equivale a 292226 cuenta TS, estamos perdiendo 101 cuentas, estas son las que no tienen padron asignado
data3 <- data3[!(data3$PADRON%in%padrones_faltantes$PADRON),] 

# data3 CONTIENE TODAS LAS TDS CON LOS PADRONES FILTRADOS EN padrones4

#write.csv(data3,"compute/data/data.csv")

#verificamos la validez
#length(unique(data3$PADRON)) #son 170899 padrones


#which(is.na(data3$CCZ)) #ninguno
#summary(data3) #no hay valores faltantes


#table(duplicated(padrones4$PADRON)) #no hay duplicados
#which(duplicated(padrones2$PADRON)) #para saber que obs son duplicados, apraentemente el padron 92216 es del CCZ 12 y 10 por eso aparece dduplicado
#table(duplicated(st_geometry(padrones4))) #sin duplicados por geometria


#---- UNIMOS LOS DATOS ESPACIALES CON LOS DATOS DE LAS TDS ----

dataSP <- left_join(padrones4,data3,by=c("PADRON"="PADRON")) #demora menos de 1min


#table(duplicated(dataSP$CUENTA_TS)) #ningun duplicado, correcto
#table(duplicated(dataSP$PADRON)) #122137 duplicados tendrian que ser 121327 , esta correcto

#table(duplicated(st_geometry(dataSP))) #same anterior
#table(st_is_valid(dataSP)) #4 false, demora un poco
#which(!(st_is_valid(dataSP))) #son las observaciones con geometria invalida

#table(st_is_empty(dataSP)) #todas false, correcto

# con st_make_valid se soluciona la geometria invalida de las obs [c(197571,  249688,  249689, 249690),]
dataSP[c(197571,  249688,  249689, 249690),] <- st_make_valid(dataSP[c(197571,  249688,  249689, 249690),]) 

#st_is_valid(dataSP[c(197571,  249688,  249689, 249690),]) #solucionado

mydata1 <- st_collection_extract(dataSP, "POLYGON") # demora aprox. 2min 

st_write(mydata1, "compute/data/dataSP.shp") # aprox. 1min


#---- FILTRADO DE OBSERVACIONES ----

# filtramos TDS que no nos interesan para los modelos
# nos interesa trabajar con las cuentas activas, y las de categoria familiar y comercial 

dataSP2 <- dataSP %>% filter(TS_CORTADA == '0') 

dataSP2 <- dataSP2 %>% filter(CATEGORIA != 'ARTICULO 91')

dataSP2 <- dataSP2 %>% filter(CATEGORIA != 'GUBERNAMENTAL')


# actualizamos los levels de CATEGORIA
dataSP2$CATEGORIA <- droplevels(dataSP2$CATEGORIA)

#str(dataSP2$CATEGORIA)
#levels(dataSP2$CATEGORIA)

# creo dataSP2.shp
#head(dataSP2)

mydata2 <- st_collection_extract(dataSP2, "POLYGON") # demora aprox. 2min 

st_write(mydata2, "compute/data/dataSP2.shp") # aprox. 1min
# son 273386 observaciones 


# filtramos TDS con BIM6 'sin generar'
dataSP3 <- dataSP2 %>% filter(BIM6 != '-1') 
# 155 TDS filtradas, nos quedamos con 273231 obs.

# actualizamos los levels de BIM6
dataSP3$BIM6 <- droplevels(dataSP3$BIM6)

#str(dataSP3$BIM6)
#levels(dataSP3$BIM6)


# creo ahora dataSP3.shp
#head(dataSP3)

mydata3 <- st_collection_extract(dataSP3, "POLYGON") # demora tipo 2min 

st_write(mydata3, "compute/data/dataSP3.shp") # aprox. 1min

#Warnings al terminar .shp
#Warning messages:
#  1: In abbreviate_shapefile_names(obj) :
#  Field names abbreviated for ESRI Shapefile driver
#  2: In clean_columns(as.data.frame(obj), factorsAsCharacter) :
#  Dropping column(s) ANTIGUE of class(es) difftime

# dataSP3 tiene 273231 obs. y removio la variable de ANTIGUEDAD
# conclusion no vale la pena crear la variable 'ANTIGUEDAD' en dataSP3 antes de hacer st_write






