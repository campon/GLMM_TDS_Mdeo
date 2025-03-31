n <- 500

# invento los datos del CCZ 1
x <- ifelse(runif(n) > 0.5 , 1, 0)
y <- rbinom(n, 1, 1/(1+exp(-(-0.5+0.7*x))))
d1 <- data.frame(x,y)

# invento los datos del CCZ 2
x <- ifelse(runif(n) > 0.5 , 1, 0)
y <- rbinom(n, 1, 1/(1+exp(-(-1.5+1.7*x))))
d2 <- data.frame(x,y)

# invento los datos del CCZ 3
x <- ifelse(runif(n) > 0.5 , 1, 0)
y <- rbinom(n, 1, 1/(1+exp(-(1+0.2*x))))
d3 <- data.frame(x,y)

# ajusto un modelo en cada CCZ
m1 <- glm(y~x, data = d1, family=binomial())
m2 <- glm(y~x, data = d2, family=binomial())
m3 <- glm(y~x, data = d3, family=binomial())

# y este es el dibujo que yo te decia
library(tidyverse)

# las estimaciones
c1 <- coef(m1)
c2 <- coef(m2)
c3 <- coef(m3)
# los intervalos
ic1 <- confint(m1)
ic2 <- confint(m2)
ic3 <- confint(m3)

datos <- data.frame(ccz  = as.factor(1:3),
                    beta = c(c1[2],c2[2],c3[2]),
                    or   = exp(c(c1[2],c2[2],c3[2])),
                    linf95 = c(ic1[2,1],ic2[2,1],ic3[2,1]),
                    lsup95 = c(ic1[2,2],ic2[2,2],ic3[2,2]))

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

# en cualquiera de los 2 casos se ve que en el CCZ 3 no se detecta asociacion entre
# X y Y mientras que en los otros 2 si. Lo otro que "mas o menos" se ve es que la 
# asociacion es distinta en cada CCZ
