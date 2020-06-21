#Seteo
rm(list=ls())
setwd("C:/Users/USUARIO/Documents/Cursos Profesionales/Especialización en R para Data Science/Clase 5_R4DS")
getwd()
dir()

#Instalando y cargando librerías

library(quantmod)
library(dplyr)
library(ggplot2)
library(tseries)
library(forecast)

#Sea AR(2):  y[t] = mu + phi[1]*y[t-1] + phi[2]*y[t-2] + Err[t]
#Fijando semilla
set.seed(700)

#Se define modelos AR (2), con descripción en un objeto de tipo list y se simula como ARIMA(2,0,0):

#donde p1 es pequeño = 0.1 y p2=[-0.9,0.9] para N=200
AR2_sm1 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.1,-0.9), sd=0.1))
AR2_sm2 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.1,-0.5), sd=0.1))
AR2_sm3 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.1,-0.2), sd=0.1))
AR2_sm4 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.1,0.2), sd=0.1))
AR2_sm5 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.1,0.5), sd=0.1))
AR2_sm6 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.1,0.9), sd=0.1))

# Graficando series de tiempo simuladas

par(mfrow = c(2,3))
# configuremos el eje Y adecuado
ylm1 <- c(min(AR2_sm1,AR2_sm2,AR2_sm3,AR2_sm4,AR2_sm5,AR2_sm6),
         max(AR2_sm1,AR2_sm2,AR2_sm3,AR2_sm4,AR2_sm5,AR2_sm6))

plot.ts(AR2_sm1, ylim = ylm1, main = "AR(2) p1 = 0.1 y p2=-0.9")
plot.ts(AR2_sm2, ylim = ylm1, main = "AR(2) p1 = 0.1 y p2=-0.5")
plot.ts(AR2_sm3, ylim = ylm1, main = "AR(2) p1 = 0.1 y p2=-0.2")
plot.ts(AR2_sm4, ylim = ylm1, main = "AR(2) p1 = 0.1 y p2=0.2")
plot.ts(AR2_sm5, ylim = ylm1, main = "AR(2) p1 = 0.1 y p2=0.5")
plot.ts(AR2_sm6, ylim = ylm1, main = "AR(2) p1 = 0.1 y p2=0.9")
dev.off()

#Autocorrelación simple

graphics.off()
par(mfrow = c(2,3))

acf(AR2_sm1,main="AR(2) p1 = 0.1 y p2 = -0.9")
acf(AR2_sm2,main="AR(2) p1 = 0.1 y p2 = -0.5")
acf(AR2_sm3,main="AR(2) p1 = 0.1 y p2 = -0.2")
acf(AR2_sm4,main="AR(2) p1 = 0.1 y p2 = 0.2")
acf(AR2_sm5,main="AR(2) p1 = 0.1 y p2 = 0.5")
acf(AR2_sm6,main="AR(2) p1 = 0.1 y p2 = 0.9")
dev.off()

#Autocorrelación parcial

graphics.off()
par(mfrow = c(2,3))

pacf(AR2_sm1,main="AR(2) p1 = 0.1 y p2 = -0.9")
pacf(AR2_sm2,main="AR(2) p1 = 0.1 y p2 = -0.5")
pacf(AR2_sm3,main="AR(2) p1 = 0.1 y p2 = -0.2")
pacf(AR2_sm4,main="AR(2) p1 = 0.1 y p2 = 0.2")
pacf(AR2_sm5,main="AR(2) p1 = 0.1 y p2 = 0.5")
pacf(AR2_sm6,main="AR(2) p1 = 0.1 y p2 = 0.9")
dev.off()


#donde p1 es grande = 0.9 y p2=[-0.9, 0.9] para N=200

AR2_lg1 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.9,-0.9), sd=0.1))
AR2_lg2 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.9,-0.5), sd=0.1))
AR2_lg3 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.9,-0.2), sd=0.1))
AR2_lg4 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.9,0.2), sd=0.1))
AR2_lg5 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.9,0.5), sd=0.1))
AR2_lg6 <- arima.sim(n=200, model=list(order=c(2,0,0), ar = c(0.9,0.9), sd=0.1))

#Graficando simulación
graphics.off()
par(mfrow = c(2,3))
# configuremos el eje Y adecuado
ylm2 <- c(min(AR2_lg1,AR2_lg2,AR2_lg3,AR2_lg4,AR2_lg5,AR2_lg6),
          max(AR2_lg1,AR2_lg2,AR2_lg3,AR2_lg4,AR2_lg5,AR2_lg6))

plot.ts(AR2_lg1, ylim = ylm2, main = "AR(2) p1 = 0.9 y p2=-0.9")
plot.ts(AR2_lg2, ylim = ylm2, main = "AR(2) p1 = 0.9 y p2=-0.5")
plot.ts(AR2_lg3, ylim = ylm2, main = "AR(2) p1 = 0.9 y p2=-0.2")
plot.ts(AR2_lg4, ylim = ylm2, main = "AR(2) p1 = 0.9 y p2=0.2")
plot.ts(AR2_lg5, ylim = ylm2, main = "AR(2) p1 = 0.9 y p2=0.5")
plot.ts(AR2_lg6, ylim = ylm2, main = "AR(2) p1 = 0.9 y p2=0.9")
dev.off()

#Autocorrelación simple

graphics.off()
par(mfrow = c(2,3))

acf(AR2_lg1,main="AR(2) p1 = 0.9 y p2 = -0.9")
acf(AR2_lg2,main="AR(2) p1 = 0.9 y p2 = -0.5")
acf(AR2_lg3,main="AR(2) p1 = 0.9 y p2 = -0.2")
acf(AR2_lg4,main="AR(2) p1 = 0.9 y p2 = 0.2")
acf(AR2_lg5,main="AR(2) p1 = 0.9 y p2 = 0.5")
acf(AR2_lg6,main="AR(2) p1 = 0.9 y p2 = 0.9")
dev.off()

#Autocorrelación parcial

graphics.off()
par(mfrow = c(2,3))

pacf(AR2_lg1,main="AR(2) p1 = 0.9 y p2 = -0.9")
pacf(AR2_lg2,main="AR(2) p1 = 0.9 y p2 = -0.5")
pacf(AR2_lg3,main="AR(2) p1 = 0.9 y p2 = -0.2")
pacf(AR2_lg4,main="AR(2) p1 = 0.9 y p2 = 0.2")
pacf(AR2_lg5,main="AR(2) p1 = 0.9 y p2 = 0.5")
pacf(AR2_lg6,main="AR(2) p1 = 0.9 y p2 = 0.9")
dev.off()

################################################################################

#Se define modelos MA (2), con descripción en un objeto de tipo list y se simula como ARIMA(0,0,2):

#donde p1 es pequeño = 0.1 y p2=[-0.9,0.9] para N=200
MA2_sm1 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.1,-0.9), sd=0.1))
MA2_sm2 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.1,-0.5), sd=0.1))
MA2_sm3 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.1,-0.2), sd=0.1))
MA2_sm4 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.1,0.2), sd=0.1))
MA2_sm5 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.1,0.5), sd=0.1))
MA2_sm6 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.1,0.9), sd=0.1))

# Graficando series de tiempo simuladas

par(mfrow = c(2,3))
# configuremos el eje Y adecuado
ylm1 <- c(min(MA2_sm1,MA2_sm2,MA2_sm3,MA2_sm4,MA2_sm5,MA2_sm6),
          max(MA2_sm1,MA2_sm2,MA2_sm3,MA2_sm4,MA2_sm5,MA2_sm6))

plot.ts(MA2_sm1, ylim = ylm1, main = "MA(2) p1 = 0.1 y p2=-0.9")
plot.ts(MA2_sm2, ylim = ylm1, main = "MA(2) p1 = 0.1 y p2=-0.5")
plot.ts(MA2_sm3, ylim = ylm1, main = "MA(2) p1 = 0.1 y p2=-0.2")
plot.ts(MA2_sm4, ylim = ylm1, main = "MA(2) p1 = 0.1 y p2=0.2")
plot.ts(MA2_sm5, ylim = ylm1, main = "MA(2) p1 = 0.1 y p2=0.5")
plot.ts(MA2_sm6, ylim = ylm1, main = "MA(2) p1 = 0.1 y p2=0.9")
dev.off()

#Autocorrelación simple

graphics.off()
par(mfrow = c(2,3))

acf(MA2_sm1,main="MA(2) p1 = 0.1 y p2 = -0.9")
acf(MA2_sm2,main="MA(2) p1 = 0.1 y p2 = -0.5")
acf(MA2_sm3,main="MA(2) p1 = 0.1 y p2 = -0.2")
acf(MA2_sm4,main="MA(2) p1 = 0.1 y p2 = 0.2")
acf(MA2_sm5,main="MA(2) p1 = 0.1 y p2 = 0.5")
acf(MA2_sm6,main="MA(2) p1 = 0.1 y p2 = 0.9")
dev.off()

#Autocorrelación parcial

graphics.off()
par(mfrow = c(2,3))

pacf(MA2_sm1,main="MA(2) p1 = 0.1 y p2 = -0.9")
pacf(MA2_sm2,main="MA(2) p1 = 0.1 y p2 = -0.5")
pacf(MA2_sm3,main="MA(2) p1 = 0.1 y p2 = -0.2")
pacf(MA2_sm4,main="MA(2) p1 = 0.1 y p2 = 0.2")
pacf(MA2_sm5,main="MA(2) p1 = 0.1 y p2 = 0.5")
pacf(MA2_sm6,main="MA(2) p1 = 0.1 y p2 = 0.9")
dev.off()


#donde p1 es grande = 0.9 y p2=[-0.9, 0.9] para N=200

MA2_lg1 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.9,-0.9), sd=0.1))
MA2_lg2 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.9,-0.5), sd=0.1))
MA2_lg3 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.9,-0.2), sd=0.1))
MA2_lg4 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.9,0.2), sd=0.1))
MA2_lg5 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.9,0.5), sd=0.1))
MA2_lg6 <- arima.sim(n=200, model=list(order=c(0,0,2), ma = c(0.9,0.9), sd=0.1))

# Graficando series de tiempo simuladas

par(mfrow = c(2,3))
# configuremos el eje Y adecuado
ylm2 <- c(min(MA2_lg1,MA2_lg2,MA2_lg3,MA2_lg4,MA2_lg5,MA2_lg6),
          max(MA2_lg1,MA2_lg2,MA2_lg3,MA2_lg4,MA2_lg5,MA2_lg6))

plot.ts(MA2_lg1, ylim = ylm2, main = "MA(2) p1 = 0.9 y p2=-0.9")
plot.ts(MA2_lg2, ylim = ylm2, main = "MA(2) p1 = 0.9 y p2=-0.5")
plot.ts(MA2_lg3, ylim = ylm2, main = "MA(2) p1 = 0.9 y p2=-0.2")
plot.ts(MA2_lg4, ylim = ylm2, main = "MA(2) p1 = 0.9 y p2=0.2")
plot.ts(MA2_lg5, ylim = ylm2, main = "MA(2) p1 = 0.9 y p2=0.5")
plot.ts(MA2_lg6, ylim = ylm2, main = "MA(2) p1 = 0.9 y p2=0.9")
dev.off()

#Autocorrelación simple

graphics.off()
par(mfrow = c(2,3))

acf(MA2_lg1,main="MA(2) p1 = 0.9 y p2 = -0.9")
acf(MA2_lg2,main="MA(2) p1 = 0.9 y p2 = -0.5")
acf(MA2_lg3,main="MA(2) p1 = 0.9 y p2 = -0.2")
acf(MA2_lg4,main="MA(2) p1 = 0.9 y p2 = 0.2")
acf(MA2_lg5,main="MA(2) p1 = 0.9 y p2 = 0.5")
acf(MA2_lg6,main="MA(2) p1 = 0.9 y p2 = 0.9")
dev.off()

#Autocorrelación parcial

graphics.off()
par(mfrow = c(2,3))

pacf(MA2_lg1,main="MA(2) p1 = 0.9 y p2 = -0.9")
pacf(MA2_lg2,main="MA(2) p1 = 0.9 y p2 = -0.5")
pacf(MA2_lg3,main="MA(2) p1 = 0.9 y p2 = -0.2")
pacf(MA2_lg4,main="MA(2) p1 = 0.9 y p2 = 0.2")
pacf(MA2_lg5,main="MA(2) p1 = 0.9 y p2 = 0.5")
pacf(MA2_lg6,main="MA(2) p1 = 0.9 y p2 = 0.9")
dev.off()







