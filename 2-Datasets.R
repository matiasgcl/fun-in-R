# 2: Trabajando con datasets (y un poco de uso de distros (pdf,cdf,...))

data(iris)
# Listar etiquetas contenido
names(iris)
# para acceder directamente a las variables desde nuestro espacio de trabajo
attach(iris)

# tabla frecuencias
table(iris$Species)

# ej burdo
vec <- c(1,1,1,0,0,0,3,3,3,3,3,2)
table(vec)

# freq relativa
table(vec)/length(vec)

# implementacion moda
moda <- function(var){
  frec.var <- table(var)
  value <- which(frec.var==max(frec.var))
  as.numeric(names(value))
}

# R incluye media que corta outliers, usando parametro trim
mean(vec,trim=0.1) # corto el 10% extremo

# la mediana tiende ser más robusta ante outliers
median(vec)

# percentiles/quintiles/cuartiles
# percentiles (todos!)
quantile(Sepal.Length,seq(0,1,0.01))
# cuartiles (25pct)
quantile(Sepal.Length,seq(0,1,0.25))
# Resumen/Summary de los datos
summary(iris)

# usando TAPPLY para analizar por sub-especies

tapply(iris$Petal.Length,iris$Species,summary)
tapply(iris$Petal.Width,iris$Species,summary)
tapply(iris$Sepal.Length,iris$Species,summary)
tapply(iris$Sepal.Width,iris$Species,summary)

# rango = max - min
max(Sepal.Length) - min(Sepal.Length)

# Varianza y SD - también sensible a outliers! 
var(Sepal.Length)
sd(Sepal.Length)

# Opción para robustez: avg absolute deviation
# AAD(x) = 1/n sum |x_i - m(x)|, m() medida de tendencia central (usualmente mediana)
aad<-function(x,fun=median){ # <- es interesante esta notación para usar otras funciones
  mean(abs(x-fun(x)))
}

# median absolute deviation
# MAD(x) = b*mediana(|x_i-m(x)|), implementado en R como mad; usa fn center (por defecto mediana, b=1.482)
mad(Sepal.Length)

# IQR (interquartile range) = Q3-Q1 (importante para boxplots)
IQR(Sepal.Length)

# Varianza y covarianzas
cov(Sepal.Length, Sepal.Width)
cov(iris[,1:4]) # 5ta col son las especies, dato no numérico.

# coeficiente de correlacion de Pearson (normalizacion correls)
cor(iris[,1:4])

# Cuando los datos son categoricos, puedo usar tablas de contingencia (para ver frecuencias)
gender<-c("Male", "Female", "Male", "Female", "Female", "Male")
studies<-c("college","postgraduate","high school",
           "postgraduate","high school","college")

table(gender,studies)

# Otros estadísticos relacionados con entender la distribución
# Skewness y Kurtosis
# Skewness -> cuantificar asimetría, 0 si dist muestral es simétrica, negativo si la cola derecha concentra más valores, positivo si la cola izq concentra más valores
# en particular, neg => mediana mayor que media, pos => mediana menor que media
library(moments)
skewness(c(1,1,2,3)) # pos
skewness(c(1,2,3,3)) # neg

# Kurtosis: medir el tipo de cola que tiene la dist muestral
# Mesokurtic: kurtosis ~ 3, dist normal.
# Leptokurtic: colas más grandes que la normal, kurtosis > 3 -> es más probable tener outliers (comparado a normal)
# Platykurtic: colas más delgadas que la normal, kurtosis < 3 -> menos probable tener outliers (comparado a normal)
test <- rnorm(1000,0,1)
plot(density(test))
kurtosis(test)
skewness(test)
test2 <- rexp(1000,1)
plot(density(test2))
kurtosis(test2) # lepto
skewness(test2)
test3 <- rbeta(1000,2,2)
plot(density(test3))
kurtosis(test3) # platy
skewness(test3)

############
# PLOTTING #
############

plot(rnorm(15,10,5),col="red",type="p",pch=1)
lines(rnorm(15,10,5),col="blue",type="p",pch=1)
lines(rnorm(15,10,5),col="green",type="b",pch=2)
title(main="My Plot")
legend("topright", c("lines","dots","both") ,
        lty=1:3, col=c("red", "blue","green"), bty='n', cex=.75)

# histogramas
hist(Sepal.Length,nclass=100) # bins

# ggplot2 grafica más bonito
library(ggplot2)
ggplot(iris, aes(x=Sepal.Length)) + geom_histogram(bins = 10, color="black", fill="white")

# density = version suavizada del histograma (kernel density estimation)
plot(density(iris$Sepal.Length),main="Density of Sepal.Length")

# hay pie charts pero se recomienda evitarlos
pie(table(iris$Species))

# BOXPLOTS: recordar las ideas de base
# rectangulo entre Q1 y Q3, linea en mediana
# altura rect = IQR
# extension de las líneas: Q1 - 1.5IQR, Q3 + 1.5IQR
# fuera de eso? outliers!
# obs: si fuera una normal, fuera de las lineas (brazos), vamos más allá de 2.698sd

boxplot(Sepal.Length,main="Boxplot Sepal.Length")
boxplot(Sepal.Length~Species,ylab="Sepal.Length") # separacion por tipo
boxplot(x=iris[,1:4],main="Boxplots Iris") # multiples boxplot en un solo gráfico

# usando ggplot
ggplot(iris, aes(x = Species, y = Sepal.Length,
                 fill = Species)) + geom_boxplot()

# scatter plots
# Sepal width vs. sepal length
plot(Sepal.Width~Sepal.Length, col=Species)
# Equivalent
plot(Sepal.Length, Sepal.Width,col=Species,
     pch=as.numeric(Species))
# We add a legend
legend('topright', levels(Species) ,
        lty=1, col=1:3, bty='n', cex=.75)

# o via ggplot
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=3,shape=4)

# todos los scatter formato matriz, util para visualizar (si se explica bien)
pairs(iris[,1:4],pch=as.numeric(iris$Species),col=iris$Species)

# 3d scatters
# install.packages("scatterplot3d",dependencies=T)
library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length,
              iris$Sepal.Width, color=as.numeric(iris$Species),
              pch=as.numeric(iris$Species))

# parallel coordinate plots (no me gustan)
library(MASS)
parcoord(iris[1:4], col=iris$Species,var.label=T)

###################
# PROBA
###################
#n!
factorial(10)
#binom(n,k)
choose(20,3)

# Usando distribuciones de probabilidad
# dxxx : density/mass function
# pxxx : cumulative distribution function
# qxxx : quantile function
# rxxx : random variate generation

# Ejemplo
# Steph Curry solo anotó 2 de 4 tiros libres el 20 de Enero de 2018
# Su estadística acumulada en tal temporada fue de 0.91
# Cual era la probabilidad, bajo esos datos, de anotar solo el 50% de tiros libres?
# Binom!, si X: numero de tiros libres anotados (sobre n lanzados) en tal partido n=4, p=0.91
# P(X=2) = binom(4,2)*p^2*(1-p)^(4-2)
choose(4,2)*0.91^2*(1-0.91)^2
# o más compactamente
dbinom(x=2,size=4,p=0.91)
help("dbinom")

# Jugando con la normal
# X~N(3,5), P(X>1) ?
# P(X>1) = 1-P(X<1) = 1-P((Z*sqrt(5)+3)<1) = 1-P(Z<(1-3)/sqrt(5)), Z ~ N(0,1)
1 - pnorm(q=(1-3)/sqrt(5))
# o
1 - pnorm(q=1,mean=3,sd=sqrt(5)) # ojo con sd = sqrt(var)
´
# X ~ N(15,5), P(13<X<18) ?
# P(13<X<18) = P(X<18) - P(X<13) 
pnorm(q=18,mean=15,sd=sqrt(5)) - pnorm(q=13,mean=15,sd=sqrt(5))
# o 
pnorm(q=(18-15)/sqrt(5)) - pnorm(q=(13-15)/sqrt(5))  

# los famosos cortes segun desplazamiento en sigma (N(0,1))
pnorm(1) - pnorm(-1)
pnorm(2) - pnorm(-2)
pnorm(3) - pnorm(-3)

# ploteos

x<-seq(-8,8,length=400)
y1<-dnorm(x,mean=0,sd=0.5)
y2<-dnorm(x,mean=0,sd=1)
y3<-dnorm(x,mean=0,sd=2)
plot(y1~x,type="l",col="red")
lines(y2~x,type="l",col="green")
lines(y3~x,type="l",col="blue")
