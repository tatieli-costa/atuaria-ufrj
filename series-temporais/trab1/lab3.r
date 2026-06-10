#===============================================================================
#                                    __   /\             /\/\   /\          /\__
#   ANÁLISE DE SÉRIES TEMPORAIS     /  \/   \        __ /    \_/  \  /\    /
#                               ___/         \__/\_/               \/  \/\/
#===============================================================================

#===============================================================================
#   Questão 1
#===============================================================================

# (a) ----------

phi <- 0.8   # |phi| >= 1: processo não estacionário
sigma2 <- 1
n <- 200

y <- rep(NA,length=200)
y[1] <- rnorm(1,0,sqrt(sigma2/(1+phi^2)))   # sorteando y utilizando a variância do processo: Var(y_t) = sigma2/(1 + phi^2)
for(t in 2:n){
  y[t] <- phi*y[t-1] + rnorm(1,0,sqrt(sigma2))
}

ts.plot(y)

# ----->

phi <- -0.8   # |phi| < 1: processo estacionário
sigma2 <- 1
n <- 200

y <- rep(NA,length=200)
y[1] <- rnorm(1,0,sqrt(sigma2/(1+phi^2)))   # sorteando y utilizando a variância do processo: Var(y_t) = sigma2/(1 + phi^2)
for(t in 2:n){
  y[t] <- phi*y[t-1] + rnorm(1,0,sqrt(sigma2))
}

ts.plot(y)   # gráfico da série temporal

# (b) ----------

acf(y)   # gráfico da função de autocorrelação (teoricamente: decaimento exponencial ou senoides amortecidas)

# (c) ----------

# rho(1) = phi*rho(0) = phi (Yule-Walker)

rho1 <- acf(y)$acf[2]   # função de autocorrelação amostral de lag h=1
phihat <- rho1   # estimativa de phi

# gamma(0) = phi*gamma(1) + sigma2 = phi*gamma(0)*rho(1) + sigma2 => sigma = gamma(0)*(1 - phi*rho(1))

gamma0 <- acf(y,type="covariance")$acf[1]   # variância amostral
sigma2hat <- gamma0*(1 - phihat*rho1)

# (d) ----------

pacf(y)   # função de autocorrelação parcial amostral (teoricamente: zero após lag h=1)

#===============================================================================
#   Questão 2
#===============================================================================

# (a) ----------

# processo estacionário se phi1 + phi2 < 1; phi2 - phi < 1; -1 < phi2 < 1

phi1 <- 0.5   # processo não estacionário
phi2 <- 0.5
sigma2 <- 1
n <- 200

y <- rep(NA,length=200)
y[1] <- rnorm(1,0,sqrt(sigma2/(1+phi^2)))   # sorteando y utilizando a variância do processo: Var(y_t) = sigma2/(1 + phi^2)
y[2] <- rnorm(1,0,sqrt(sigma2/(1+phi^2)))   # sorteando y utilizando a variância do processo: Var(y_t) = sigma2/(1 + phi^2)
for(t in 3:n){
  y[t] <- phi1*y[t-1] + phi2*y[t-2] + rnorm(1,0,sqrt(sigma2))
}

ts.plot(y)

# ----->

phi1 <- 0.2   # processo estacionário
phi2 <- 0.5
sigma2 <- 1
n <- 200

y <- rep(NA,length=200)
y[1] <- rnorm(1,0,sqrt(sigma2/(1+phi^2)))   # sorteando y utilizando a variância do processo: Var(y_t) = sigma2/(1 + phi^2)
y[2] <- rnorm(1,0,sqrt(sigma2/(1+phi^2)))   # sorteando y utilizando a variância do processo: Var(y_t) = sigma2/(1 + phi^2)
for(t in 3:n){
  y[t] <- phi1*y[t-1] + phi2*y[t-2] + rnorm(1,0,sqrt(sigma2))
}

ts.plot(y)

# (b) ----------

acf(y)   # gráfico da função de autocorrelação (teoricamente: decaimento exponencial ou senoides amortecidas)

# (c) ----------

# rho(1) = phi1*rho(0) + phi2*rho(1)
# rho(2) = phi1*rho(1) + phi2*rho(0)
# phi = Gamma^{-1}*rho

rho1 <- acf(y)$acf[2]   # função de autocorrelação amostral de lag h=1
rho2 <- acf(y)$acf[3]   # função de autocorrelação amostral de lag h=2
rhovec <- c(rho1,rho2)
Gamma <- matrix(c(1,rho1,rho1,1),2,2)   # matriz Gamma

phivec <- solve(Gamma)%*%rhovec   # vetor com as estimativas de phi1 e phi2

# gamma(0) = phi1*gamma(1) + phi2*gamma(2) + sigma2 = phi1*gamma(0)*rho(1) + phi2*gamma(0)*rho(2) + sigma2
# => sigma = gamma(0)*(1 - phi1*rho(1) - phi2*rho(2))

gamma0 <- acf(y,type="covariance")$acf[1]   # variância amostral
sigma2hat <- gamma0*(1 - phivec[1]*rho1 - phivec[2]*rho2)

# (d) ----------

pacf(y)   # função de autocorrelação parcial amostral (teoricamente: zero após lag h=2)

#===============================================================================
#   Questão 3
#===============================================================================


y <- scan("serie.txt")

ts.plot(y)

# covariáveis

t <- 1:length(y)
t2 <- t^2

# ajustando o modelo de tendência polinomial

aj <- lm(y ~ t + t2)

# analisando os resíduos (série livre de tendência)

res <- aj$res

acf(res)

pacf(res)

# rho(1) = phi*rho(0) = phi (Yule-Walker)

rho1 <- acf(res)$acf[2]   # função de autocorrelação amostral de lag h=1
phihat <- rho1   # estimativa de phi

# gamma(0) = phi*gamma(1) + sigma2 = phi*gamma(0)*rho(1) + sigma2 => sigma = gamma(0)*(1 - phi*rho(1))

gamma0 <- acf(res,type="covariance")$acf[1]   # variância amostral
sigma2hat <- gamma0*(1 - phihat*rho1)


