#===============================================================================
#                                    __   /\             /\/\   /\          /\__
#   ANÁLISE DE SÉRIES TEMPORAIS     /  \/   \        __ /    \_/  \  /\    /
#                               ___/         \__/\_/               \/  \/\/
#===============================================================================

#===============================================================================
#   Questão 1
#===============================================================================

# (a) ----------

yn <- scan("dolar.txt")

y <- yn[1:60]   # considerando somente as primeiras 60 observações

ts.plot(y)

dy <- diff(y)   # diferenciando a série

ts.plot(dy)
acf(dy)   # autocorrelação
pacf(dy)   # autocorrelação parcial

aj1 <- arima(y,order=c(0,1,0))   # modelo identificado
aj2 <- arima(y,order=c(1,1,0))   # outros ajustes --->
aj3 <- arima(y,order=c(0,1,1))
aj4 <- arima(y,order=c(1,1,1))   # <---

AIC(aj1)
AIC(aj2)
AIC(aj3)
AIC(aj4)
BIC(aj1)
BIC(aj2)
BIC(aj3)
BIC(aj4)

# modelo escolhido: ARIMA(0,1,0) (passeio aleatório)

# analisando resíduos --->

ts.plot(aj1$res)
acf(aj1$res)
Box.test(aj1$res)   # teste de Box-Pierce para indenpendência (H0)
Box.test(aj1$res,type="Ljung")   # teste de Ljung-Box para indenpendência (H0)

qqnorm(aj1$res)   # verificando normalidade dos resíduos
qqline(aj1$res)

shapiro.test(aj1$res)   # teste de Shapiro- Wilk para normalidade (H0)

# (b) e (c) ----------

pred <- predict(aj1,n.ahead=7)   # predizendo valores para h=1,...,7

ypred <- pred$pred   # valores preditos (dim=7)
qinf <- ypred - qnorm(.975)*pred$se   # IC --->
qsup <- ypred + qnorm(.975)*pred$se  # <---

ypredv <- c(rep(NA,l=60),ypred)   # vetor de valores preditos (dim=67)
qinfv <- c(rep(NA,l=60),qinf)  # IC --->
qsupv <- c(rep(NA,l=60),qsup)   # <---

ts.plot(yn,ylim=c(min(y),max(qsup)))   # série completa
lines(ypredv,type="o",col=4)   # valores preditos
lines(qinfv,lty=2,col=2)   # limite inferior IC
lines(qsupv,lty=2,col=2)   # limite superior IC

#===============================================================================
#   Questão 2
#===============================================================================

# (a) e (b) ----------

t <- 1:60
t2 <- t^2

reg <- lm(y~t+t2)   # ajustando modelo de regressão (tendência polinomial de ordem 2)

ts.plot(y)
lines(reg$fit,col=2)   # plotando o ajuste

ts.plot(reg$res)
acf(reg$res)   # autocorrelação dos resíduos
pacf(reg$res)  # autocorrelação parcial dos resíduos

X <- cbind(t,t2)   # matriz de desenho sem intercepto (o modelo terá intercepto)
aj1 <- arima(y,xreg=X,order=c(1,0,0))   # modelo identificado
aj2 <- arima(y,xreg=X,order=c(2,0,0))   # outros ajustes --->
aj3 <- arima(y,xreg=X,order=c(0,0,1))
aj4 <- arima(y,xreg=X,order=c(1,0,1))
aj5 <- arima(y,xreg=X,order=c(2,0,1))   # <---

AIC(aj1)
AIC(aj2)
AIC(aj3)
AIC(aj4)
AIC(aj5)
BIC(aj1)
BIC(aj2)
BIC(aj3)
BIC(aj4)
BIC(aj5)

# modelo escolhido: y_t = beta0 + beta1*t + Beta2*t^2 + epsilon_t, epsilon_t ~ AR(1)

# analisando resíduos --->

ts.plot(aj1$res)
acf(aj1$res)
Box.test(aj1$res)   # teste de Box-Pierce para indenpendência (H0)
Box.test(aj1$res,type="Ljung")   # teste de Ljung-Box para indenpendência (H0)

qqnorm(aj1$res)   # verificando normalidade dos resíduos
qqline(aj1$res)

shapiro.test(aj1$res)   # teste de Shapiro- Wilk para normalidade (H0)

# (c) e (d) ----------

tpred <- 61:67
t2pred <- tpred^2
Xpred <- cbind(tpred,t2pred)   # matriz de desenho da previsão
pred <- predict(aj1,n.ahead=7,newxreg=Xpred)   # prevendo valores para h=1,...,7

ypred <- pred$pred
qinf <- ypred - qnorm(.975)*pred$se
qsup <- ypred + qnorm(.975)*pred$se

ypredv <- c(rep(NA,l=60),ypred)
qinfv <- c(rep(NA,l=60),qinf)
qsupv <- c(rep(NA,l=60),qsup)

yaj <- aj1$coef[2] + aj1$coef[3]*t + aj1$coef[4]*t2   # valores ajustados

ts.plot(yn,ylim=c(min(y),max(qsup)))   # série completa
lines(ypredv,type="o",col=4)   # valores preditos
lines(qinfv,lty=2,col=2)   # limite inferior IC
lines(qsupv,lty=2,col=2)   # limite superior IC
lines(yaj,lty=3,col="firebrick")   # valores ajustados

#===============================================================================
#   Questão 3
#===============================================================================

# (a) ----------

yn <- scan("lavras.txt")

y <- yn[1:360]    # considerando as primeiras 360 observações

ts.plot(y)
acf(y)

dy <- diff(y,lag=12)   # diferença sazonal de lag 12

ts.plot(dy)
acf(dy,lag.max=36)
pacf(dy,lag.max=36)

aj1 <- arima(y,seasonal=list(order=c(0,1,1),period=12))   # modelo identificado
aj2 <- arima(y,seasonal=list(order=c(0,1,2),period=12))   # outros ajustes --->
aj3 <- arima(y,seasonal=list(order=c(1,1,0),period=12))
aj4 <- arima(y,order=c(1,0,0),seasonal=list(order=c(0,1,1),period=12))
aj5 <- arima(y,order=c(0,0,1),seasonal=list(order=c(0,1,1),period=12))   # <---

AIC(aj1)
AIC(aj2)
AIC(aj3)
AIC(aj4)
AIC(aj5)
BIC(aj1)
BIC(aj2)
BIC(aj3)
BIC(aj4)
BIC(aj5)

# modelo escolhido: SARIMA(0,0,0)x(0,1,1)_12

# analisando resíduos --->

ts.plot(aj1$res)
acf(aj1$res)
Box.test(aj1$res)   # teste de Box-Pierce para indenpendência (H0)
Box.test(aj1$res,type="Ljung")   # teste de Ljung-Box para indenpendência (H0)

qqnorm(aj1$res)   # verificando normalidade dos resíduos
qqline(aj1$res)

shapiro.test(aj1$res)   # teste de Shapiro- Wilk para normalidade (H0)

# (b) e (c) ----------

pred <- predict(aj1,n.ahead=24)   # predizendo valores para h=1,...,24

ypred <- pred$pred
qinf <- ypred - qnorm(.975)*pred$se
qsup <- ypred + qnorm(.975)*pred$se

ypredv <- c(rep(NA,l=360),ypred)
qinfv <- c(rep(NA,l=360),qinf)
qsupv <- c(rep(NA,l=360),qsup)

ts.plot(yn[331:384],ylim=c(min(qinf),max(y)))   # série completa a partir de t=331
lines(ypredv[331:384],type="o",col=4)   # valores preditos
lines(qinfv[331:384],lty=2,col=2)   # limite inferior IC
lines(qsupv[331:384],lty=2,col=2)   # limite superior IC

#===============================================================================
#   Questão 4
#===============================================================================

# (a) e (b) ----------

t <- 1:360

x1 <- cos(2*pi/12*t)
x2 <- sin(2*pi/12*t)

reg <- lm(y~x1+x2)   # ajustando modelo de regressão (harmônica)

ts.plot(y)
lines(reg$fit,col=2)   # plotando o ajuste

ts.plot(reg$res)
acf(reg$res)
pacf(reg$res)

X <- cbind(x1,x2)   # matriz de desenho sem intercepto (o modelo terá intercepto)
aj1 <- arima(y,xreg=X)   # modelo identificado
aj2 <- arima(y,xreg=X,order=c(1,0,0))   # outros ajustes --->
aj3 <- arima(y,xreg=X,order=c(0,0,1))
aj4 <- arima(y,xreg=X,order=c(1,0,1))   # <---

AIC(aj1)
AIC(aj2)
AIC(aj3)
AIC(aj4)
BIC(aj1)
BIC(aj2)
BIC(aj3)
BIC(aj4)

# modelo escolhido: y_t = beta0 + beta1*cos(2*pi/12*t) + beta2*sen(2*pi/12*t) + epsilon_t, epsilon ~ RB

# analisando resíduos --->

ts.plot(aj1$res)
acf(aj1$res)
Box.test(aj1$res)   # teste de Box-Pierce para indenpendência (H0)
Box.test(aj1$res,type="Ljung")   # teste de Ljung-Box para indenpendência (H0)

qqnorm(aj1$res)   # verificando normalidade dos resíduos
qqline(aj1$res)

shapiro.test(aj1$res)   # teste de Shapiro- Wilk para normalidade (H0)

# (c) e (d) ----------

tpred <- 361:384
x1pred <- cos(2*pi/12*tpred)
x2pred <- sin(2*pi/12*tpred)
Xpred <- cbind(x1pred,x2pred)   # matriz de desenho da previsão
pred <- predict(aj1,n.ahead=24,newxreg=Xpred)   # predizendo valores para h=1,...,24

ypred <- pred$pred
qinf <- ypred - qnorm(.975)*pred$se
qsup <- ypred + qnorm(.975)*pred$se

ypredv <- c(rep(NA,l=360),ypred)
qinfv <- c(rep(NA,l=360),qinf)
qsupv <- c(rep(NA,l=360),qsup)

yaj <- aj1$coef[1] + aj1$coef[2]*x1 + aj1$coef[3]*x2   # valores ajustados

ts.plot(yn[331:384],ylim=c(min(qinf),max(y)))   # série completa a partir de t=331
lines(ypredv[331:384],type="o",col=4)   # valores preditos
lines(qinfv[331:384],lty=2,col=2)   # limite inferior IC
lines(qsupv[331:384],lty=2,col=2)   # limite superior IC
lines(yaj[331:384],lty=3,col="firebrick")   # valores ajustados
