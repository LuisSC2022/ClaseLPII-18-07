# modificacion por luis
# Distribuciones Continuas ------------------------------------------------


# Distribución Normal -----------------------------------------------------
# dnorm(x, mean, sd) - densidad
# pnorm(x, mean, sd) - acumulada
# qnorm(F(x), mean, sd) - quantil
dnorm()
# Ejemplo 1: Calule las siguientes probalidades:
# a) p(z<=1) = p(z=0) + p(z=1)
# SOLUCIÓN:

pnorm(1)
dnorm(1)

# b) p(z=1.965)
pnorm(1.96)
qnorm(0.975)

# c) p(-2<z<1.75) = p(z<=1.75) - p(z<-2)
pnorm(1.75) - pnorm(-2)

# Ejemplo 2: El tiempo requerido para ensamblar una pieza mecánica es una v.a.
# cuya distribución es normal con media igual a 12,9 mm y desv. est. a 2 minutos,
# ¿Cuál es la probabilidad de que una pieza sea ensamblada en:
# a) en menos de 11.5 mm?
# b) entre 11 y 14.8 mm?
# c) es más que 11.8 mm?
# c) exactamente 11mm?

# SOLUCIÓN:
# z = (x-u)/desv. est. 
# a)
# p(x<11.5)

z1 = (11.5-12.9)/2
pnorm(z1)

# b) 
# p(11<=z<=14.8)
z1 = (11-12.9)/2
z2 = (14.8-12.9)/2

pnorm(z2)-pnorm(z1)

# c)
# p(x>11.8) = 1 - p(x<=11.8)
z1 = (11.8-12.9)/2
1 - pnorm(z1)

# d)
# p(x = 11) = p(z<=11.5) - p(10.5<=z)
z1 = (10.5-12.9)/2
z2 = (11.5-12.9)/2
pnorm(z2) - pnorm(z1)

# Ejemplo 3:
# F(0.95)
qnorm(0.95)

# F(0.975)
qnorm(0.975)


# Gráfica de la distribución normal ---------------------------------------

# Gráfica de la distribución normal - densidad
x <- seq(-4, 8, 0.1); x
plot(x, dnorm(x, mean = 0, sd = 1), type = "l", col = "red", 
     main = "Función de densidad de la Distribución Normal")

# Gráfica de la distribución normal - acumulada
plot(x, pnorm(x, mean = 0, sd = 1), type = "l", col = "red",
     main = "Función de acumulada de la Distribución Normal")

# Ejercicio 1: 
# a) p(z>2)
# b) p(0.05<z<1.96)

# Ejercicio 2: Los hornos eléctricos fabricados por una compañía tienen una duración
# promedio de 15000 hrs. y una desv. est. igual a 2500 hrs.
# a) Si el fabricante promete reponer todo horno que falle antes de las 7500 hrs.
# ¿Qué proporción de sus hornos tendrá que reponer?
# b) Si da como tiempo de garantía 8500 hrs. ¿Qué proporción de los hornos tendrá
# que reponer?
# c) Si sólo quiere reponer máximo el 1% de sus hornos, ¿Qué tiempo de garantía
# tendrá que dar?

# Ejercicio 3: Un fabricante de escapes para automóviles desea garantizar su
# producto durante un período igual a la duración del vehículo. El fabricante sabe que el
# tiempo de duración de su producto es una v.a. con distribución normal con una
# vida media de 3 años y una desv. est. = 6 meses. Si el costo de reemplazo es de 20
# soles por unidad, ¿cuál es el costo total de reemplazo para: 
# a) los primeros 2 años
# b) los primeros 18 meses, si se instalan 20000 unidades?

# Distribución T-Student --------------------------------------------------

# Ejemplo 1: Si t es una v.a. con 10 g.l. halle las probabilidades:
# dt(x, gl) - densidad
# pt(x, gl) - acumulada
# qt(F(x), gl) - quantil

# a) p(t<1.812)
pt(1.812,10)

# b) p(t>-1.372)
1 - pt(-1.372,10)

# c) p(-1.812<t<2.764)
pt(2.764,10) - pt(-1.812,10)

# d) p(t=1.465)
dt(1.465,10)

#e) F(0.9499) 
qt(0.9499,10)


# Gráfica de la distr. T-Student ------------------------------------------

x <- seq( -5, 5, by = 0.1); x
y <- dnorm( x ); y

plot( function(x) dt( x, df = 2 ), -5, 5,
      col = "red", type = "l", lwd = 2,
      main = "Función densidad t de Student df = 2" )

plot( function(x) pt( x, df = 2 ), -5, 5,
      col = "red", type = "l", lwd = 2,
      main = "Función acumulada t de Student df = 2" )

# Ejercicio 1: Si la v.a. t tiene 20 g.l. Halle las siguientes probabilidades:
# a) P(t < 2,528)
# b) P(t > -2,086)
# c) P(t = -1,725)
# d) P(-2,845 < t < 2,845)


# Distribución de Chi Cuadrada --------------------------------------------
# dchisq(x, gl) - densidad
# pchisq(x, gl) - acumulada
# qchisq(F(x), gl) quantil

# Ejemplo 1: Si x es una v.a. X^2 con 17 g.l., calcule:
# a) P(X^2 < 7,56)
# b) P(X^2 > 27,59)
# c) P(6,408 < X^2 < 27,59)
# d) P(X^2 = 64.58)
# e) F(x) = 0.95


# SOLUCIÓN:
# a)
pchisq(7.56,17)

# b)
1 - pchisq(27.59,17)

# c)
pchisq(27.59,17) - pchisq(6.408,17)

# d) 
dchisq(64.58,17)

# e)
qchisq(0.95,17)

gl <- 4; gl
x<-seq(0,gl*3,length=5000); x 
plot(x,dchisq(x,gl), 
     main=c("Función de densidad de la Distribución Chi Cuadrada"), 
     sub=paste("gl=",gl),type="l",col="blue")

plot(x,pchisq(x,gl), 
     main=c("Función de acumulada de la Distribución Chi Cuadrada"), 
     sub=paste("gl=",gl),type="l",col="red")

# Ejercicio 1: Resuelva los sgtes ejercicios
# a) P(X^2 > 44,314)
# b) P(X^2 < 11,524)
# c) P(X^2 > 37,65)
# d) P(10,52 < X2 < 46,93)
# e) F(0.98)



# Distribución de Fisher --------------------------------------------------

# df(x, gl1, gl2)
# pf(x, gl1, gl2)
# qf(x, gl1, gl2)

# Ejemplo 1: Sea x una v.a. que tiene distribución F con n = 10 y m = 12 g.l., 
# hallar los valores para las siguientes probabilidades:
# a) P(F < 2.75)
# b) P(F > 4.3)
# c) P(0.438 < F < 2.19)
# d) P(F = 2.41)
# e) F(0.95)

# SOLUCIÓN:

# a)
pf(2.75,10,12)

# b)
1 - pf(4.3,10,12)

# c)
pf(2.19,10,12) - pf(0.438,10,12)

# d)
df(2.41,10,12)

# e)
qf(0.95,10,12)


# Gráfica de la Distribución Fisher ---------------------------------------
gl1<-9; gl1
gl2<-15; gl2
x<-seq(0,6,length=5000); x 
plot(x,df(x,gl1,gl2), main=c("Función de densidad de la Distribución Fisher"), 
     sub=paste("gl=",gl1,",",gl2),type="l",col="blue")

plot(x,pf(x,gl1,gl2), main=c("Función de acumulada de la Distribución Fisher"), 
     sub=paste("gl=",gl1,",",gl2),col="red")


# Ejercicio 1: Sea X una v.a. que tiene una distribución F con n = 12 y m = 10 g.l.
# , hallar las probabilidades de las siguientes preguntas:
# a) P(F < 2.91)
# b) P(F > 4.71)
# c) P( 0.4566 < F < 2.28)
# c) F(0.975)
# c) P(F=2.451)


qchisq(0.82,20)
pchisq(25.7128,20)

1-pt(-2.015,5)
qt(0.2,10)
#Los gl >0
1-pt(-2.015,5.68)

#F
1-pf(5.79,2,5)
qf(0.95,8,6)
#Los grados de libertad puede ser cualqueir valor real
qf(0.95,30.3984,35.57854)


qf(0.95,1,9999999999999999)

