#Ejecicio 7 de la tarea A MNPyR 2024-2
library(broom)
library(lmtest)
library(car)
library(nortest)

rm(list = ls(all.names = TRUE))
gc()

setwd("C:/Users/pedro/Documents/Estadística/2")
datos0=read.csv("performance.csv")
datos=read.csv("performance.csv")

View(datos)

plot(api00~ell,data=datos)

"Inciso I" 

#Primero ajustamos el modelo de regresión lineal simple.

fit=lm(api00~ell,data=datos)
summary(fit)

#Comenzamos verificando los supuestos:



#Prueba de Linealidad

par(mar=c(4,5,1,1))
par(mfrow=c(1,1))
plot(fit, 1)



datos$api00=datos$api00+1
datos$ell=datos$ell+1

boxTidwell(api00~ell,data=datos)


#Prueba de Homocedasticidad

par(mar=c(4,5,1,1))
par(mfrow=c(1,1))
plot(fit, 3)

#Notamos en la gráfica que casi todos los puntos están dispuestos de forma aleatoria;
#estos estan dispuestos aproximadamente en forma un rectángulo, pero con sus lineas en forma curva.dado esto ,
#que deberíamos realizar más pruebas antes de corroborar o no el supuesto.

datosfit=augment(fit)
par(mar=c(4,5,1,1))
par(mfrow=c(1,1))
plot(datosfit$.fitted, datosfit$.std.resid)

#Observamos  que los puntos están acumulados cerca del eje x del 0, 
#por otro lado hay varios puntos que hacen que no sean tan uniformes por lo que sera nesesario realizar mas pruebas
#, por lo que usamos otras pruebas a continuación.

lmtest::bptest(fit)

#El p-value es de .09543

car::ncvTest(fit)

#El p-value es de .12879

#Enlas pruebas anteriores, el p-value es mayor a .05,0.9543 y 0.12979 respectivamente
#por lo tanto, no se rechaza H0,
#y, dicho de otra forma se concluye que no hayamos evidencia en contra del supuesto de 
#homocedasticidad.



#Prueba de Normalidad

par(mar=c(4,5,1,1))
par(mfrow=c(1,1))
plot(fit, 2)

#En la gráfica notamos que algunos puntos siguen la recta propuesta, sin embargo muchos se mueve
#bastante en partitular los de las esquinas.

shapiro.test(datosfit$.std.resid) #p_value=0.007596

nortest::lillie.test(datosfit$.std.resid) #p-value=0.01955

#En ambos casos, el p-value es menor a .05, por lo que rechazamos H0.

#Sin embargo, como n es grande (n=400), entonces por el teorema del límite central que funciona para n>100
#podemos ver a estos como valores normales concluyendo asi que no hay evidencia en contra de la normalidad.


"Inciso II"

#Prueba de Linealidad
#dado que no pudimos decir nada respecto a la linealidad propondremos
# una posible linealidad , y para eso nos 
#ayudamos de una transformación boxTidwell:
boxTidwell(api00~ell,data=datos)
#La salida nos da 0.43447, por lo que tomamos lambda= 1/2:

datos$Xprima=datos$ell^(1/2)
fit2=lm(api00~Xprima, data=datos)
summary(fit2)

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(fit2, 1)


datosfit2=augment(fit2)
par(mar=c(4,5,1,1))
par(mfrow=c(1,1))
plot(datosfit2$.fitted, datosfit2$.std.resid)

#En esta gráfica,los puntos se encuentran dispersos pero de forma mas uniforme
#alrededor del eje Y.

boxTidwell(api00~Xprima, data=datos)



par(mar=c(4,5,1,1))
par(mfrow=c(1,1))
plot(fit2, 3)

datosfit2=augment(fit2)
par(mar=c(4,5,1,1))
par(mfrow=c(1,1))
plot(datosfit2$.fitted, datosfit2$.std.resid)

summary(powerTransform(fit2))



lmtest::bptest(fit2)

#El p-value es de 0.1284

car::ncvTest(fit2)

#El p-value es de 0.13297


par(mar=c(4,5,1,1))
par(mfrow=c(1,1))
plot(fit2, 2)

#En la gráfica notamos que LOS punto se comportan similar a la recta
#es decir que hay indicios que puedan tener un comportamiento normal.
shapiro.test(datosfit2$.std.resid) #p_value=0.1166

nortest::lillie.test(datosfit2$.std.resid) #p-value=0.3063

#En ambos casos, el p-value de 0.1166 y o.3060 son mayores a 0.05
#por lo que no descaertamos Ho





"Inciso IV"

summary(fit2)



"Inciso V"

t.test(datos$ell, datos$api00, mu=0, alternative="less")

