rm(list = ls(all.names = TRUE))
gc()

#Datos University
#Valor total de la universidad (empresa) vs los costos de la colegiatura  

library(Ecdat)
Datos <- University
help(University)
#Y nassets
#X stfees

head(Datos)
str(Datos)

library(latex2exp)
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(Datos$stfees, Datos$nassets, xlab = TeX("$stfees$"), ylab=TeX("$nassets$") )

fit=lm(nassets~stfees, data=Datos)
summary(fit)

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit, 3)


library(broom)
Datosfit=augment(fit)
head(Datosfit)
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1)) 
plot(Datosfit$.fitted, Datosfit$.std.resid, xlab = TeX("$\\widehat{y}$"), ylab=TeX("$e_{s}$")   )



### Pruebas de hipótesis
### H0: varianza no depende de forma lineal en x vs  Ha: varianza depende de forma lineal en x
### Se busca no rechazar, es decir,
### que sea plausible (no se encontró evidencia en contra) asumir que la varianza no depende de forma lineal de x
### i.e. p-value mayor a significancia.
#Usa residuales studentilizados
library(lmtest)
lmtest::bptest(fit)

#Usa residuales estandarizados
library(car)
car::ncvTest(fit)

summary(Datos$nassets)

#### Uso de transformaciones Box-Cox
#### Función powerTransform del paquete car
#### Cuando la variable "y" es estrictamente positiva

#Se prefiere un valor de lambda conocido para no complicar mucho la interpretaci?n
summary(powerTransform(fit))

#El intervalo sugiere que lambda=.2 podría ser una opción (notar que se rechaza lambda=0 y 1)

Datos$nassets_lambda0=bcPower(Datos$nassets, .2)
Datos$nassetsBC=(Datos$nassets^(.2)-1)/.2


par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1)) 
plot(Datos$stfees, Datos$nassetsBC, xlab = TeX("$stfees$"), ylab=TeX("$(y^{.2}-1)/.2$") )


fit2=lm(nassetsBC~stfees, data=Datos)
summary(fit2)

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit2, 3)

lmtest::bptest(fit2)
car::ncvTest(fit2)

summary(powerTransform(fit2))


#Para datos negativos o con ceros se puede usar una constante gamma positiva para trabajar con valores positivos 
#powerTransform(,  family="bcnPower")
summary(powerTransform(fit,  family="bcnPower"))
Datos$nassetsBCalt=bcnPower(Datos$nassets, lambda=0, gamma=16677.21)
fit3=lm(nassetsBCalt~stfees, data=Datos)
plot(Datos$stfees, Datos$nassetsBCalt, xlab = TeX("$stfees$"), ylab=TeX("$ln(z)$") )

plot(fit3, 3)

lmtest::bptest(fit3)
car::ncvTest(fit3)

#Una opción alternativa es transformar los datos sumandole la constante positiva antes de usar la transformación BoxCox simple


# Nota. En este modelo se puede presentar como resultado
# una gráfica con el ajuste del modelo, pero para facilidad del usuario
# en la escala original, que corresponde a la mediana, Med(y;x)


par(mfrow=c(1,2)) 
par(mar=c(4, 5, 1, 1))

fit2yprima <- function(X2) {fit2$coef[1]+ fit2$coef[2]*X2}
fit2y <- function(X2) {((fit2$coef[1]+ fit2$coef[2]*X2)*.2+1)^5}

# En la escala transformada la curva (recta) corresponde
# a la estimación de E(y*;x)=Med(y*;x)
plot(Datos$stfees,Datos$nassetsBC, xlab = TeX("$stfees$"), ylab=TeX("$y^{*}$") )
curve(fit2yprima, from = min(Datos$stfees), to = max(Datos$stfees),
      col = "red", add = T)
# En la escala original la curva corresponde
# a la estimación de Med(y;x)
plot(Datos$stfees, Datos$nassets, xlab = TeX("$stfees$"), ylab=TeX("$nassets$") )
curve(fit2y, from = min(Datos$stfees), to = max(Datos$stfees),
      col = "red", add = T)

