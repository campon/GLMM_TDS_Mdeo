# Ejemplo CAR
n <- 50
W <- matrix(0,n,n)

# relleno vecinos al azar
for (i in 1:n){
  nro_vec <- rpois(1,3)
  vec     <- sample(n, nro_vec)
  W[vec,i] <- W[i, vec] <- 1
}

# por las dudas qeu haya metido algun 1 en la diagonal
diag(W)<-0

D <- diag(apply(W, 1, sum))

alpha <- 0.9  # autocorrelation parameter
tau   <- 1    # precision de los spatial random effects

# precision matrix
Q <- tau*(D - alpha*W)

# covariance matrix
Q1 <- solve(Q)

# spatial random effects
library(MASS)
phi <- mvrnorm(1, rep(0,n), Q1)

# creo la Y para c/u de las n "areas"
b0 <- -2
b1 <- 1.3
x <- runif(n)
eta <- b0 + b1*x + phi

# al aplicar la transformacion logistica
p <- 1/(1 + exp(-eta))

# finalmente, sorteamos una bernoulli para cada valor de p
m <- 1
Y <- rbinom(n, m, p)


# ajustemos el modelo con brms
library(brms)
size <- rep(m,n)
datos <- data.frame(id=1:n,Y,size,x)

# fit logistic regression
fit0 <- brm(Y|trials(size) ~ x, 
            data = datos,
            family = binomial(),
            iter=3000, warmup=2500)
summary(fit0)


# fit a ICAR model
rownames(W) <- 1:n
fit1 <- brm(Y|trials(size) ~ x + car(W, gr = id, type = 'icar'), 
           data = datos, data2 = list(W = W),
           family = binomial(),
           iter=3000, warmup=2500) 
summary(fit1)

# model comparison
fit0 <- add_criterion(fit0, criterion = c("loo", "waic"))
fit1 <- add_criterion(fit1, criterion = c("loo", "waic"))
loo_compare(fit0, fit1, criterion = "loo")
loo_compare(fit0, fit1, criterion = "waic")
model_weights(fit0, fit1)

# cual predice mejor?
# comparamos Y con Y_hat

# reg. logistica
y_hat_0 <- ifelse(fitted(fit0)[,1]>0.2,1,0)
table(Y,y_hat_0)

# reg logistica espacial
y_hat_1 <- ifelse(fitted(fit1)[,1]>0.2,1,0)
table(Y,y_hat_1)


# fit a CAR model
rownames(W) <- 1:n
fit2 <- brm(Y|trials(size) ~ x + car(W, gr = id, type = 'escar'), 
           data = datos, data2 = list(W = W),
           family = binomial(),
           iter=3000, warmup=2500) 
summary(fit2)

fit2 <- add_criterion(fit1, criterion = c("loo", "waic"))
loo_compare(fit0, fit1, fit2, criterion = "loo")
loo_compare(fit0, fit1, fit2, criterion = "waic")

fitted