rm(list = ls(all.names = TRUE))
gc()

### Ejemplo sobre el uso de la regresión ponderada

setwd("~/GitHub/Notas 2024-2/MNPyR")
Datos = read.csv("initech.csv")

#Relación entre el salario y los años de experiencia de los trabajadores
#compañía Initech
#Y salario (salary)
#X años de experiencia (years)

head(Datos)
str(Datos)

library(latex2exp)
par(mfrow=c(1,2)) 
par(mar=c(4, 5, 1, 1))
plot(Datos$years, Datos$salary, xlab = TeX("$X=years$"), ylab=TeX("$salary$") )
plot((Datos$years+10)^2, Datos$salary, xlab = TeX("$X'=(years+10)^2$"), ylab=TeX("$salary$") )


Datos$Xprima=(Datos$years+10)^2
fit=lm(salary~Xprima, data=Datos)
summary(fit)

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit, 3)

lmtest::bptest(fit)
car::ncvTest(fit)


library(broom)
Datosfit=augment(fit)
head(Datosfit)
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,2)) 
plot(Datosfit$.fitted, Datosfit$.std.resid, xlab = TeX("$\\widehat{y}$"), ylab=TeX("$e_{s}$")   )
plot(Datosfit$Xprima, Datosfit$.std.resid, xlab = TeX("$X'$"), ylab=TeX("$e_{s}$")   )


### Usamos los residuales (errores observados) al cuadrado
Datosfit$res2=Datosfit$.resid^2
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,2)) 
plot(Datosfit$Xprima, Datosfit$res2, xlab = TeX("$X'$"), ylab=TeX("$e^{2}$") )
plot(Datosfit$Xprima^2, Datosfit$res2, xlab = TeX("$X'^2$"), ylab=TeX("$e^{2}$") )

### En general, para observar el comportamiento es mejor agrupar sobre intervalos de x y sacar promedios
### En este caso, hay varios valores de una misma x, así que sólo promediaremos

library(tidyverse)
DatosfitAgrup= Datosfit %>% group_by(Xprima) %>% summarise(res2media=mean(res2))
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,2)) 
plot(DatosfitAgrup$Xprima, DatosfitAgrup$res2media, xlab = TeX("$X'$"), ylab=TeX("$e^{2}$") )
plot(DatosfitAgrup$Xprima^2, DatosfitAgrup$res2media, xlab = TeX("$X'^2$"), ylab=TeX("$e^{2}$") )




fitpond <- lm(salary~Xprima,weights = 1/(Xprima), data=Datos)
summary(fitpond)

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fitpond, 3)

car::ncvTest(fitpond)

### bptest no considera los errores ponderados, no usar lmtest::bptest(fitpond) cuando la regresión es ponderada

library(broom)
Datosfitpond=augment(fitpond)
head(Datosfitpond)
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,2)) 
plot(Datosfitpond$.fitted, Datosfitpond$.std.resid, xlab = TeX("$\\widehat{y}$"), ylab=TeX("$e_{s}$")   )
plot(Datosfitpond$Xprima, Datosfitpond$.std.resid, xlab = TeX("$X'$"), ylab=TeX("$e_{s}$")   )



fitpond2 <- lm(salary~Xprima,weights = 1/(Xprima^2), data=Datos)
summary(fitpond2)

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fitpond2, 3)

car::ncvTest(fitpond2)

### bptest no considera los errores ponderados, no usar lmtest::bptest(fitpond) cuando la regresión es ponderada

library(broom)
Datosfitpond2=augment(fitpond2)
head(Datosfitpond2)
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,2)) 
plot(Datosfitpond2$.fitted, Datosfitpond2$.std.resid, xlab = TeX("$\\widehat{y}$"), ylab=TeX("$e_{s}$")   )
plot(Datosfitpond2$Xprima, Datosfitpond2$.std.resid, xlab = TeX("$X'$"), ylab=TeX("$e_{s}$")   )




# Gráfica con resultados
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1)) 
fitpond2y <- function(X2) {fitpond2$coef[1]+ fitpond2$coef[2]*(X2+10)^2}
plot(Datos$years, Datos$salary, xlab = TeX("$X=years$"), ylab=TeX("$salary$") )
curve(fitpond2y, from = min(Datos$years), to = max(Datos$years),
      col = "red", add = T)
# Dado que aquí no se transformó a y, la recta corresponde a la estimación
# de E(y;x).

