
# Distribuciones Discreta ------------------------------------------------


# Distribución Binomial ---------------------------------------------------

# Ejemplo 1: La probabilidad de que cierto componente resista una prueba de impacto
# es de 4/5. Encuentre la probabilidad de que 3 de los 5 componentes siguientes
# resistan la prueba de impacto.

# SOLUCIÓN:
# dbinom(x,n,p) densidad
# pbinom(q,n,p) acumulada
p <- 4/5
x <- 3
n <- 5

sol1 <- round(dbinom(x, n, p),4); sol1

# Ejemplo 2: La probabilidad de que un paciente se recupere con satisfacción de una
# operación quirúrgica después de una accidente es de 0.9; si 15 personas
# accidentadas se someten a dicha operación, ¿Cuál es la probabilidad de que
# al menos uno se recupere a satisfacción?

# SOLUCIÓN:
p <- 0.9
n <- 15

# p (x>=1) = 1 - p(x=0)
x <- 0

sol1 <- 1 - round(dbinom(x,n,p),4); sol1

q <- 1 - p

sol2 <- round(1 - pbinom(q,n,p),4); sol2
# Ejemplo 3: Si la probabilidad de que cierta columna falle ante una carga axial
# es de 0.05, ¿Cuál es la probabilidad de que entre 6 de estas columnas:
# a) a lo más fallen 2?
# b) al menos fallen 2?
# SOLUCIÓN:

# a)
p <- 0.05
n <- 6
# p(x<=2) = p(x=0) + p(x=1) + p(x=2)
x0 <- 0
x1 <- 1
x2 <- 2

sol1 <- round(dbinom(x0,n,p) + dbinom(x1,n,p) + dbinom(x2,n,p), 4); sol1 

q <- 1 - p

sol2 <- round(pbinom(x2,n,p),4); sol2

# b)
p <- 0.05
n <- 6

# p(x>=2) = 1 - p(x=0) + p(x=1)

x0 <- 0
x1 <- 1

sol3 <- 1 - round(dbinom(x0,n,p)+ dbinom(x1,n,p), 4); sol3

x2 <- 2
q <- 1 - p

sol4 <- round(pbinom(x2,n,p),4); sol3

# Gráfica de Distr. Binomial ----------------------------------------------

# Gráfica para la densidad

# Creamos una secuencia de 50 números, ordenados, de uno en uno. 
x <- seq(0,50,by = 1)
x
# vector con 50 valores binomialmente distribuidos con  p=0.5
y <- dbinom(x,50,0.5)
y
# para la gráfica utilizamos plot
plot(x,y,col="blue", pch="*", 
     main = "Función de densidad de la Distribución Binomial")

# Gráfica para la acumulada

# vector con 50 valores binomialmente distribuidos
y_cdf <- pbinom(x,50,0.5)
y_cdf
# Para el ejemplo utilizamos la gráfica de plot
plot(x,y_cdf, main = "Función de acumulada de la Distribución Binomial")

# Ejercicio 1: Un fabricante de computadores asegura que sólo el 10% de sus 
# unidades requiere de ajuste durante el año de garantía, ¿Cuál es la probabilidad
# de que 5 de las 10 computadoras compradas requieran ajuste en el período de
# garantía?


# Ejercicio 2: La probabilidad de que el motor de un avión falle es de 0.1. 
# Si el avión tiene 4 motores que funcionan independientes, y para una vuelo
# exitoso se requiere que por lo menos 2 de los 4 motores funciones, ¿Cuál es la
# probabilidad de:
# a) un vuelo exitoso?
# b) que el vuelo no sea exitoso?


# Distribución de Poisson -------------------------------------------------

# Ejemplo 1: Se sabe que el número promedio de camiones que llegan a un terminal
# terrestres durante el día es de 10, las instalaciones del terminal pueden atender
# como máximo 15 camiones al día. ¿Cuál es la probabilidad de que la capacidad
# de atención del terminal sea superado en un día cualquiera?

# SOLUCIÓN:
# dpois(x,lambda) densidad
# ppois(x,lambda) acumulada
# p(x>15) = 1 - p(x<=15)
x <- 15
y <- 10

sol1 <- 1 - ppois(x,lambda = y); sol1

# Ejemplo 2: En cierto proceso de fabricación en el que se producen artículos de
# de porcelana ocurren defectos que ocasionan problemas en las ventas, el fabricante
# asegura que en promedio uno de cada 1000 artículos producidos es defectuosos.
# ¿Cuál es la probabilidad de que una caja de 3000 artículos haya:
# a) exactamente 3 artículos con defectuosos?
# b) al menos un artículo con defecto?

# SOLUCIÓN:
# a)
n <- 3000
y <- n * (1/1000); y
# p(x=3)
x <- 3

sol1 <- round(dpois(x, lambda = y),4); sol1

# b)
# p(x>=1) = 1 - p(x=0)
x <- 0
sol1 <- round(1 - ppois(x,y),4); sol1

# Ejemplo 3: Una compañía compra cantidades muy grandes de componentes electrónicos,
# la decisión para aceptar o rechazar un lote de componentes se toma en base a
# una muestra aleatoria de 100 unidades. Si el lote se rechaza al encontrar tres
# o más unidades defectuosas en la muestra, ¿Cuál es la probabilidad de rechazar
# un lote, si contiene un:
# a) 0.5% de defectuosos?
# b) 1.5% de defectuosos?

# SOLUCIÓN:
y <- 100 * (0.005); y

# p(x>=3) = 1 - p(x<=2)
x <- 2

sol1 <- round(1 - ppois(x,y),4); sol1

# Gráfica de la distribución de Poisson

x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 
       15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27)

# Gráfica de densidad
plot(dpois(x, lambda = 15.2), main = "Función de densidad de la Distribución Poisson")

# Gráfica de acumulada
plot(ppois(x, lambda = 15.2), main = "Función de acumulada de la Distribución Poisson")

# Ejercicio 1: El número de clientes que llegan a un banco es en promedio 60
# por hora, ¿Cuál es la probabilidad de que lleguen 2 clientes en:
# a) un minuto?
# b) dos minutos?

# Ejercicio 2: Al inspeccionar la aplicación de estaño por un proceso electrónico
# continuo, se descubre en promedio 0.2 imperfecciones por mínimo, calcule las 
# siguientes probabilidades:
# a) de encontrar una imperfección en 5 minutos.
# b) de encontrar 2 imperfecciones en 10 minutos.
# c) 3 imperfecciones en 15 minutos.


