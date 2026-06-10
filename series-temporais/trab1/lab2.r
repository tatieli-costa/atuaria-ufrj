#===============================================================================
#                                    __   /\             /\/\   /\          /\__
#   ANÁLISE DE SÉRIES TEMPORAIS     /  \/   \        __ /    \_/  \  /\    /
#                               ___/         \__/\_/               \/  \/\/
#===============================================================================

#===============================================================================
#   Questão 1
#===============================================================================

serie1 <- read.table("serie1.txt",header=TRUE)

y <- serie1[,2]

x <- serie1[,1]

t <- seq(1,length(y))

ts.plot(y)   # gráfico da série temporal (comando padrão)

plot(x,y,type="l")   # gráfico da série temporal com as datas

# (a) ----------

# modelo proposto: y_t = beta0 + beta1*t + epsilon_t

aj <- lm(y~t)   # ajuste do modelo de tendência linear

beta0hat <- aj$coef[1]   # estimativas dos parâmetros
beta1hat <- aj$coef[2]

f <- beta0hat + beta1hat*t   # série suavizada
f <- aj$fit   # série suavizada (comando direto)

ts.plot(y)
lines(f,col=2,lwd=2)

# (b) ----------

r <- y - f   # resíduos; série livre de tendência
r <- aj$res   # resíduos (comando direto)

ts.plot(r)

acf(r,lag.max=50)   # gráfico da função de autocorrelação estimada para os resíduos

# (c) ----------

mm <- function(y,s,q){      # função de suavização por média móveis
  n <- length(y)
  f <- rep(NA,l=n)   # vetor de NAs
  for(t in (s+1):(n-q)){
    f[t] <- mean(y[(t-s):(t+q)])
  }
  return(f)
}

ymm2 <- mm(y,2,2)   # suavização com 2 termos
ymm5 <- mm(y,5,5)   # suavização com 5 termos
ymm20 <- mm(y,20,20)   # suavização com 20 termos

ts.plot(y)
lines(ymm2,col=2,lwd=2)
lines(ymm5,col=4,lwd=2)
lines(ymm20,col=6,lwd=2)

legend(0,120,legend=c("y","MM 2","MM 5","MM 20"),col=c(1,2,4,6),lwd=c(1,2,2,2),lty=1)

# (d) ----------

n <- length(y)   # tamanho da série

r <- y - ymm5   # resíduos; série livre de tendência

ts.plot(r)

acf(r[-c(1:5,(n-5):n)],lag.max=50)   # gráfico da função de autocorrelação estimada para os resíduos

#===============================================================================
#   Questão 2
#===============================================================================

serie2 <- read.table("serie2.txt",header=TRUE)

y <- serie2[,2]

x <- serie2[,1]

t <- seq(1,length(y))

ts.plot(y)   # gráfico da série temporal (comando padrão)

n <- length(y)

plot(y,type="l",axes=FALSE)   # gráfico da série temporal com as datas
axis(1,labels=x[seq(1,n,by=6)],at=seq(6,n,by=6))
axis(2)
box()

# (a) ----------

# modelo proposto: y_t = a0 + a1*cos(2*pi*w*t) + a2*sen(2*pi*w*t) + epsilon_t

p <- 12   # período
w <- 1/p  # frequência

x1 <- cos(2*pi*w*t)
x2 <- sin(2*pi*w*t)

aj <- lm(y~x1+x2)   # ajuste do modelo de regressão harmônica

a0hat <- aj$coef[1]   # estimativas dos parâmetros
a1hat <- aj$coef[2]
a2hat <- aj$coef[3]

f <- a0hat + a1hat*cos(2*pi*w*t) + a2hat*sin(2*pi*w*t)   # série suavizada
f <- aj$fit   # série suavizada (comando direto)

ts.plot(y)
lines(f,col=2,lwd=2)

# (b) ----------

r <- y - f   # resíduos; série livre de tendência
r <- aj$res   # resíduos (comando direto)

ts.plot(r)

acf(r,lag.max=50)   # gráfico da função de autocorrelação estimada para os resíduos

# (c) ----------

a <- n/p   # número de períodos

ys <- rep(NA,l=p)
i <- 0:(a-1)   # contador de períodos
for(j in 1:p){
  ys[j] <- mean(y[i*p+j])   # calculando a médias sazonais
}

S <- ys - mean(ys)   # estimativa da sazonalidade (centralizada)

r <- y - S   # série livre de sazonalide

ts.plot(y)
lines(mean(y) + rep(S,a),col=2,lwd=2)   # note que y = média + sozonalidade

ts.plot(r)

acf(r,lag.max=50)

#===============================================================================
#   Questão 3
#===============================================================================

temp <- read.table("temperatura.txt",header=TRUE)

y <- temp[,2]

x <- temp[,1]

t <- seq(1,length(y))

ts.plot(y)   # gráfico da série temporal (comando padrão)

n <- length(y)   # tamanho da série

plot(y,type="l",axes=FALSE)   # gráfico da série temporal com as datas
axis(1,labels=x[seq(1,n,by=20)],at=seq(1,n,by=20))
axis(2)
box()

# (a) ----------

# modelo proposto: y_t = beta0 + beta1*t + beta2*t^2 + beta3*t^3 + beta4*t^4 + beta5*t^5 + epsilon_t

x1 <- t^2
x2 <- t^3
x3 <- t^4
x4 <- t^5

aj <- lm(y~t+x1+x2+x3+x4)   # ajuste do modelo de polinomial de grau 4

beta0hat <- aj$coef[1]   # estimativas dos parâmetros
beta1hat <- aj$coef[2]
beta2hat <- aj$coef[3]
beta3hat <- aj$coef[4]
beta4hat <- aj$coef[5]
beta5hat <- aj$coef[6]

f <- beta0hat + beta1hat*t + beta2hat*t^2 + beta3hat*t^3 + beta4hat*t^4  + beta5hat*t^5   # série suavizada
f <- aj$fit   # série suavizada (comando direto)

ts.plot(y)
lines(f,col=2,lwd=2)

# (b) ----------

r <- y - f   # resíduos; série livre de tendência
r <- aj$res   # resíduos (comando direto)

ts.plot(r)

acf(r,lag.max=50)   # gráfico da função de autocorrelação estimada para os resíduos

# (c) ----------

se <- function(y,alpha){
  n <- length(y)
  f <- rep(NA,l=n)
  f[1] <- y[1]
  for(t in 2:n){
    f[t] <- alpha*y[t] + (1-alpha)*f[t-1]
  }
  return(f)
}

ymm <- mm(y,15,0)   # mms
yse <- se(y,0.1)   # ses

ts.plot(y)
lines(ymm,col=6,lwd=2)
lines(yse,col=2,lwd=2)

legend(0,10,legend=c("y","MMS","SES"),col=c(1,6,2),lwd=c(1,2,2),lty=1)

# (d) ----------

r1 <- y - ymm   # resíduos; série livre de tendência
r2 <- y - yse

ts.plot(r1)
ts.plot(r2)

acf(r1[-c(1:15,(n-15):n)],lag.max=50)   # gráfico da função de autocorrelação estimada para os resíduos
acf(r2,lag.max=50)   # gráfico da função de autocorrelação estimada para os resíduos

