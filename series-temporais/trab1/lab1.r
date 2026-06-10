#===============================================================================
#                                    __   /\             /\/\   /\          /\__
#   ANÁLISE DE SÉRIES TEMPORAIS     /  \/   \        __ /    \_/  \  /\    /
#                               ___/         \__/\_/               \/  \/\/
#===============================================================================

#===============================================================================
#   Questão 1
#===============================================================================

ts.plot(AirPassengers)

ts.plot(lynx)

ts.plot(Nile)

ts.plot(UKgas)

#===============================================================================
#   Questão 2
#===============================================================================

# (b) ----------

y <- rnorm(1000,0,1)  # gerando dados

ts.plot(y)

# (c) ----------

myacf <- function(y,h){   # função que calcula as funções de autocovariância e autocorrelação amostrais
  acov <- rep(NA,length=h+1)
  acor <- rep(NA,length=h+1)
  n <- length(y)
  ybarra <- mean(y)
  acov0 <- 1/n*sum((y - ybarra)^2)
  for(i in 0:h){
    yaux1 <- y[1:(n-i)]
    yaux2 <- y[(1+i):n]
    acov[i+1] <- 1/n*sum((yaux1 - ybarra)*(yaux2 - ybarra))
  }
  acor <- acov/acov0
  return(list("acov"=acov,"acor"=acor))
}

auto <- myacf(y,30)   # calculando a autocorrelação para h=1,2, ..., 30
auto <- auto$acor
plot(0:30,auto,type="h",xlab="h",ylab=expression(hat(rho)(h)))   # gráfico da autocorrelação

acf(y)   # função do R que plota (calcula) a função de autocorrelação amostral

# (d) ----------

yaux1 <- y[1:999]
yaux2 <- y[2:1000]

plot(yaux1,yaux2,xlab=expression(y[t-1]),ylab=expression(y[t]))

#===============================================================================
#   Questão 3
#===============================================================================

# (b) ----------

# y_t = phi*y_{t-1} + epsilon_t, epsilon_t ~ N(0,sigma2)

phi <- 0.8
sigma2 <- 1
y <- rep(NA,l=1000)   # gerando dados
y[1] <- rnorm(1,0,sqrt(sigma2/(1-phi^2)))
for(t in 2:1000){
  y[t] <- phi*y[t-1] + rnorm(1,0,1)
}

ts.plot(y)

auto <- myacf(y,30)
auto <- auto$acor

plot(0:30,auto,type="h",xlab="h",ylab=expression(hat(rho)(h)))   # gráfico da autocorrelação
lines(seq(0,30),phi^seq(0,30),col="firebrick")

# (c) ----------

yaux1 <- y[1:999]   # y_{t-1}
yaux2 <- y[2:1000]   # y_t

plot(yaux1,yaux2,xlab=expression(y[t-1]),ylab=expression(y[t]))

#---

yaux1 <- y[1:998]   # y_{t-2}
yaux2 <- y[3:1000]   # y_t

plot(yaux1,yaux2,xlab=expression(y[t-2]),ylab=expression(y[t]))

#===============================================================================
#   Questão 4
#===============================================================================

# (b) ----------

# y_t = epsilon_t + theta*epsilon_{t-1}, epsilon_t ~ N(0,sigma2)

theta <- 2
sigma2 <- 1
y <- rep(NA,l=1000)   # gerando dados
e <- rep(NA,l=1000)
e0 <- rnorm(1,0,sqrt(sigma2))
e[1] <- rnorm(1,0,sqrt(sigma2))
y[1] <- theta*e0 + e[1]
for(t in 2:1000){
  e[t] <- rnorm(1,0,sqrt(sigma2))
  y[t] <- theta*e[t-1] + e[t]
}

ts.plot(y)

auto <- myacf(y,30)
auto <- auto$acor

plot(0:30,auto,type="h",xlab="h",ylab=expression(hat(rho)(h)))   # gráfico da autocorrelação
abline(h=theta/(1+theta^2),col=2,lty=2)
text(28,0.55,labels=expression(theta/(1+theta^2)),col=2)

# (c) ----------

yaux1 <- y[1:999]   # y_{t-1}
yaux2 <- y[2:1000]   # y_t

plot(yaux1,yaux2,xlab=expression(y[t-1]),ylab=expression(y[t]))

#---

yaux1 <- y[1:998]   # y_{t-2}
yaux2 <- y[3:1000]   # y_t

plot(yaux1,yaux2,xlab=expression(y[t-2]),ylab=expression(y[t]))



