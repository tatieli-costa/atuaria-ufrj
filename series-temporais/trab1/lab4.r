#===============================================================================
#                                    __   /\             /\/\   /\          /\__
#   ANÁLISE DE SÉRIES TEMPORAIS     /  \/   \        __ /    \_/  \  /\    /
#                               ___/         \__/\_/               \/  \/\/
#===============================================================================

#===============================================================================
#   Questão 1
#===============================================================================

# (a) ----------

y <- scan("co2.txt")

ts.plot(y)

# (b) ----------

z <- y - mean(y)   # centralizando o série temporal

acf(z)   # autocorrelação
pacf(z)   # autocorrelação parcial

aj1 <- arima(z,order=c(2,0,0),include.mean=FALSE)   # modelo identificado
aj2 <- arima(z,order=c(3,0,0),include.mean=FALSE)
aj3 <- arima(z,order=c(1,0,0),include.mean=FALSE)
aj4 <- arima(z,order=c(1,0,1),include.mean=FALSE)
aj5 <- arima(z,order=c(2,0,1),include.mean=FALSE)

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

# modelo escolhido: ARIMA(2,0,0) = AR(2)

# (c) ----------

# analisando resíduos --->

ts.plot(aj1$res)
acf(aj1$res)
Box.test(aj1$res)   # teste de Box-Pierce para indenpendência (H0)
Box.test(aj1$res,type="Ljung")   # teste de Ljung-Box para indenpendência (H0)

qqnorm(aj1$res)   # verificando normalidade dos resíduos
qqline(aj1$res)

shapiro.test(aj1$res)   # teste de Shapiro- Wilk para normalidade (H0)

#===============================================================================
#   Questão 2
#===============================================================================

# (a) ----------

y <- scan("icv.txt")

ts.plot(y)

z <- log(y)

ts.plot(z)

# (b) ----------

dz <- diff(z)
ts.plot(dz)

dz2 <- diff(dz)
ts.plot(dz2)

acf(dz2)   # autocorrelação
pacf(dz2)   # autocorrelação parcial

aj1 <- arima(z,order=c(0,2,1),include.mean=FALSE)   # modelo identificado
aj2 <- arima(z,order=c(0,2,2),include.mean=FALSE)
aj3 <- arima(z,order=c(1,2,1),include.mean=FALSE)
aj4 <- arima(z,order=c(1,2,2),include.mean=FALSE)
aj5 <- arima(z,order=c(0,2,3),include.mean=FALSE)

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

# modelo escolhido: ARIMA(0,2,2)

# (c) ----------

# analisando resíduos --->

ts.plot(aj2$res)
acf(aj2$res)
Box.test(aj2$res)   # teste de Box-Pierce para indenpendência (H0)
Box.test(aj2$res,type="Ljung")   # teste de Ljung-Box para indenpendência (H0)

qqnorm(aj2$res)   # verificando normalidade dos resíduos
qqline(aj2$res)

shapiro.test(aj2$res)   # teste de Shapiro- Wilk para normalidade (H0)

#===============================================================================
#   Questão 3
#===============================================================================

# (a) ----------

y <- Nile

ts.plot(y)

# (b) ----------

dy <- diff(y)
ts.plot(dy)

acf(dy)   # autocorrelação
pacf(dy)   # autocorrelação parcial

aj1 <- arima(y,order=c(0,1,2),include.mean=FALSE)   # modelo identificado
aj2 <- arima(y,order=c(0,1,3),include.mean=FALSE)
aj3 <- arima(y,order=c(1,1,2),include.mean=FALSE)
aj4 <- arima(y,order=c(1,1,1),include.mean=FALSE)

AIC(aj1)
AIC(aj2)
AIC(aj3)
AIC(aj4)
BIC(aj1)
BIC(aj2)
BIC(aj3)
BIC(aj4)

# modelo escolhido: ARIMA(1,1,1)

# (c) ----------

# analisando resíduos --->

ts.plot(aj4$res)
acf(aj4$res)
Box.test(aj4$res)   # teste de Box-Pierce para indenpendência (H0)
Box.test(aj4$res,type="Ljung")   # teste de Ljung-Box para indenpendência (H0)

qqnorm(aj4$res)   # verificando normalidade dos resíduos
qqline(aj4$res)

shapiro.test(aj4$res)   # teste de Shapiro- Wilk para normalidade (H0)

#===============================================================================
#   Questão 3
#===============================================================================

# (a) ----------

y <- Nile

ts.plot(y)

# há uma aparente mudança de nível depois de 1898 (t=28); essa foi a época do início da construção da repressa de Assuã no Rio Nilo

n <- length(y)

x <- rep(0,l=100)
x[29:n] <- 1   # variável de intervenção para indicar os tempos em que houve mudança no nível da série temporal (função degrau)

reg <- lm(y~x)   # ajustando modelo de regressão

# analisando resíduos --->

ts.plot(reg$res)
acf(reg$res)
Box.test(reg$res)   # teste de Box-Pierce para indenpendência (H0)
Box.test(reg$res,type="Ljung")   # teste de Ljung-Box para indenpendência (H0)

qqnorm(reg$res)   # verificando normalidade dos resíduos
qqline(reg$res)

shapiro.test(reg$res)   # teste de Shapiro- Wilk para normalidade (H0)

# resíduos se comportam como ruído branco; não há necessiade de ajustar um modelo ARMA

