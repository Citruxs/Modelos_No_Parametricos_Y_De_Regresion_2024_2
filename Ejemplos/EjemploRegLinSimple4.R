rm(list = ls(all.names = TRUE))
gc()

### Ejemplo 4. Verificación sobre linealidad/homocedasticidad

setwd("~/GitHub/Notas 2024-2/MNPyR")
Datos = read.csv("ejemplo4.csv")

head(Datos)
str(Datos)
library(latex2exp)
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(Datos$x, Datos$y, xlab = TeX("$x$"), ylab=TeX("$y$") )



fit=lm(y~x, data=Datos)
summary(fit)


#R tiene una gráfica propia para verificar linealidad
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit, 1)


###Buscamos una posible transformación
library(car)
boxTidwell(y~x, data=Datos)

#La salida es el estimador de la potencia 1.98 aprox 2
#Test Ho: lambda=1 vs Ha:lambda != 1.
#Si no se rechaza, entonces es plausible considerar el modelo con X directamente
#Si se rechaza, entonces conviene usar el estimador de lambda para transformar X 
# u otra transformación
#Aquí se rechaza H0.
Datos$Xprima=Datos$x^2
fit2=lm(y~Xprima, data=Datos)
summary(fit2)

#R tiene una gráfica propia para verificar linealidad
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit2, 1)

boxTidwell(y~Xprima, data=Datos)
#Ya no se rechaza H0

#¿se cumple el supuesto de homocedasticidad?

#Homocedasticidad
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit2, 3)

library(lmtest)
lmtest::bptest(fit2)

summary(powerTransform(fit2))

