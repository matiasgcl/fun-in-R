# 3: Inferencia

# Sampling dist of the sample mean
# Dada una poblacion de tamaño N, ¿Como será la distribución de medias muestrales (de tamaño n < N) para todas las muestras posibles?

# primero: definamos la desv est muestral
sd.p = function(x){sd(x)*sqrt((length(x)-1)/length(x))}

# datos sintéticos
pop <- c(2,3,4,5,6)

# generemos todas las muestras posibles
library(tidyverse)
samp_size <- 2
samples <- as_tibble(permutations(length(pop), samp_size, pop, repeats.allowed=T))

# calcular medias para cada una y añadir a samples
samples <- samples %>% rowwise() %>% mutate(sample_mean=mean(c(V1,V2)))
# CLT en accion!
ggplot(samples, aes(x=sample_mean)) + geom_histogram(bins = 10, color="black", fill="white")

ggplot(data.frame(pop), aes(x=pop)) + geom_histogram(bins = 5, color="black", fill="white")

# calculemos cosas relacionadas a las medias muestrales
mean(samples$sample_mean)
sd.p(samples$sample_mean)

#se = sigma/sqrt(n), se teorico
sd.p(pop)/sqrt(samp_size) # = sd.p(samples$sample_mean)

# media muestral es un buen estimador de la media de la poblacion y el error estandar puede ser calculado via sd de la poblacion y el tamaño de muestra
# sampling dist de la media será normal si las muestras son suficientes (n~30,40 segun autores)

########################################
# INT CONFIANZA PARA MEDIA, VAR CONOCIDA
########################################
# C_n = (mu_n - z_alpha/2*sigma/sqrt(n), mu_n + z_alpha/2*sigma/sqrt(n))
# para nivel alpha
alpha <- 0.05
xbar <- 5
sigma <- 2
n <- 20
se <- sigma/sqrt(n)
error <- qnorm(1-alpha/2)*se 
left <- xbar - error
right <- xbar + error
left
right

# Y si no conozco ni media ni var?
# Si data es sabidamente normal, estimo sigma usando s; intervalos de confianza para la media muestral se construyen mejor usando una t-student (esp si tamaño pequeño de muestra)
x<-seq(-8,8,length=400)
y1<-dnorm(x)
y2<-dt(x=x,df=1)
y3<-dt(x=x,df=10)
y4<-dt(x=x,df=350)
plot(y1~x,type="l",col="green")
lines(y2~x,type="l",col="blue")
lines(y3~x,type="l",col="black")
lines(y4~x,type="l",col="red")

# Ejemplo
data(iris)
alpha<-0.05
n<-length(iris$Petal.Length)
xbar<-mean(iris$Petal.Length)
s<-sd(iris$Petal.Length)
se<-s/sqrt(n)
error<-qt(p=1-alpha/2,df=n-1)*se
left<-xbar-error
left
right<-xbar+error
right

# R lo implementa
test<-t.test(iris$Petal.Length,conf.level=0.95)
test$conf.int

# Caso de proporcion en poblacion
# 1219 personas responden que votarían por candidato A en muestra de 3532 personas.
# Calcular un intervalo de 95% de confianza para la proporcion de votantes
# hat(p) = 1219/3235 = 0.345
# zalpha/2 = 1.96
# Cn = (0.329, 0.361) (a mano)
# En R es directo
prop<-prop.test(1219,3532,correct=FALSE)
prop$conf.int

########################
# BOOTSTRAP
########################
# idea: usar los datos disponibles como poblacion total, hacer submuestreos para determinar potencial distribución

myboot<-function(x,fun,nRuns,sampleSize,alpha){
  values<-vector()
  for(i in 1:nRuns){
    samp.i<-sample(x, size = sampleSize, replace = T)
    values[i]<-fun(samp.i)
  }
  point.est <-fun(x)
  se <- sd(values)
  l.CI <- quantile(values, alpha/2)
  u.CI <- quantile(values, 1-alpha/2)
  return(c("Point Estimate"=point.est,
           "Standard error"=se,
           "Lower CI limit" = l.CI,
           "Upper CI limit" = u.CI))
}

######################################################
# TEST DE HIPOTESIS
######################################################
# Ejemplo bebés
# H0: mu = 3000
# H1: mu > 3000 (hipotesis del estudio es que las guaguas pesan más de 3kg)
# ONE SIDED TEST
library(UsingR)
data(babyboom)
hist(babyboom$wt)
xbar <- mean(babyboom$wt)
# queremos calcular la proba de obtener una muestra con media de al menos xbar ASUMIENDO H0
# CLT: Xbar ~ N(mu,sigma^2/sqrt(n))
# si H0 es verdad: mu = 3000
# sigma = 500 conocido
# n dado por la muestra (44)
# calculemos...
mu0 <- 3000
sd <- 500
n <- nrow(babyboom)
se <- sd/sqrt(n)
se
se^2
# Calculemos la probabilidad deseada (asumiendo H0!!! i.e. distribución N(3000,se^2))
# (o p valor)
# P(Xbar>media_muestral) = 1 - P(Xbar < media_muestral) 
1-pnorm(xbar, mean=mu0, sd = se)
# alternativa R (pasandose a N(0,1))
Z.score <- (xbar-mu0)/se
p.value <- 1-pnorm(Z.score)
p.value
alpha <- 0.01
p.value <= alpha
# Resultado es Verdadero: i.e. hay evidencia significativa contra H0 al nivel 1%
# "La probabilidad de los datos es baja si asumimos H0" (relativa la nivel de significancia alpha)
# (Y si p > alpha?) "La evidencia contra H0 no es significativa al nivel alpha%"

# Y SI SIGMA NO ES CONOCIDO?
# Si asumimos datos normales y tamaño muestra pequeño: t-student

s<-sd(babyboom$wt)
se.t<-s/sqrt(n)
T.sta<-(xbar-mu0)/se.t
p.value<-1-pt(T.sta,df = n-1)
p.value
# tambien se rechaza, pero ahora el p valor es un poco más grande
# pq? colas de t.

# R permite hacer todo expedito
t.test(x = babyboom$wt,mu = 3000, alternative = "greater",conf.level = 1-alpha)

# Test visto como regiones críticas.
# La pregunta es si el estadístico del test está dentro la región de rechazo o no
# caso normal
crit <- qnorm(1-alpha)
Z.score >= crit
# caso t
crit2<-qt(1-alpha, df = n-1)
T.sta>=crit2

##################
# TWO SIDED TESTS
##################

# Y si en lugar de mu > 3000, quiero saber si = 3000 y != 3000 (i.e >3000 y <3000) ?
# H0: mu=3000
# H1: mu>3000 o mu<3000
# Los calculos anteriores son iguales, pero ahora el nivel alpha se reparte a los dos lados de la normal/t
# i.e. region crítica menor 2.5% y mayor 2.5%
crit.left <- qnorm(alpha/2)
crit.right <- qnorm(1-alpha/2)
Z.score < crit.left | Z.score > crit.right # sigo en alguna de las regiones? SI
critt.left <- qt(alpha/2, df=n-1)
critt.right <- qt(1-alpha/2, df=n-1)
T.sta < critt.left | T.sta > critt.right # sigo en alguna de las regiones? SI

# Otra alternativa (two sided)
# calcular region de confianza nivel alpha para la media muestral
# el intervalo es la region de aceptacion y rechazo H0 si m0=3000 NO está en el intervalo
left.conf<-xbar-qt(p=1-alpha/2,n-1)*se.t
right.conf<-xbar+qt(p=1-alpha/2,n-1)*se.t
mu0 >= left.conf | mu0 <= right.conf
# ojo, los int de conf y el p valor siempre coinciden cuando hay significancia estadística

# calculo del p-valor
pvalue<-pnorm(-Z.score)+(1-pnorm(Z.score))
# o equivalentemente
2*pnorm(-abs(Z.score))
# t
2*pt(-abs(T.sta),df=n-1)

# R lo hace solo
t.test(x=babyboom$wt,mu=3000,alternative="two.sided",conf.level=1-alpha)

# Unpaired two samples test (no son la misma cantidad en cada sexo)
# H0: mu_boys - mu_girls = 0
# H1: mu_boys - mu_girls != 0
# Asumiendo H0: X1-X2 ~ N(0,sigma^2_1/n1 + sigma^2_2/n2)
# Si sigmas no son conocidos, estimamos usando (s1,s2) y se construye el estadístico
# T = (X1-X2)/sqrt(s^2_1/n1 + s^2_2/n2) ~ t
# si los grupos son igual de grandes y de misma varianza: df = n1+n2-2
# en el caso de los datos babyboom esto no es cierto (ver boxplots)
# => Welch t-test:
# df = (s^2_1/n1 + s^2_2/n2)^2 /( (s^2_1/n_1)/(n_1-1) + (s^2_2/n_2)/(n_2-1) )

t.test(babyboom$wt~babyboom$gender)
# p-valor=0.1665 ! no hay evidencia para rechazar H0

# Paired T-test
# esencialmente para testear antes-despues
# cada muestra tiene dos valores.
# como se trabaja? calculo diferencia d para cada par
# calculo media ~d y sd s_d de las diferencias
# comparo ~d con 0 como en los test anteriores.

# el estadístico aquí es T = ~d/s_d/sqrt(n)
# df = n-1
# Ejemplo: ~d = 0 y ~d!=0
before <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
after <-c(392.9, 393.2, 345.1,393, 434, 427.9, 422, 383.9, 392.3, 352.2)
t.test(after, before, paired = TRUE, alternative = "two.sided")
# se rechaza H0 !

##############
# ERRORES
##############
# Tipo I: Rechazo H0 cuando era cierta "falsa alarma"
# Tipo II: H0 es falso pero no tengo evidencia estadística para rechazarlo "miss"
# Se definen:
# P(error Tipo I) = alpha, P(error Tipo II) = beta
# Si fijamos alpha = 0.05, a largo plazo cometeremos de errores tipo I el 5% de las veces.
# Valor estándar para beta=0.2, i.e. aceptamos que podemos fallar el 20% del tiempo en detectar un efecto cuando efectivamente existe
# Se define la potencia estadística de un test como
# power = 1 - beta
# verosimilitud de encontrar resultado positivo dado que existe (efecto)
# Como mitigar errores? tipo I: reducir alpha, tipo II: n más grande. (trade-off entre ambos!)

########################
# REGRESIONES
########################
d <- read.csv("howell1.csv")
cor(d)
# excluir muy jovenes
d2 <- d[ d$age >= 18 , ]
cor(d2)
# se pierde la 'potencial' correlacion entre edad y altura! (natural!)
# trabajemos con altura y peso: altura(peso) ~ b0 + b1*peso

reg1<-lm(height~weight,d2)
reg1
reg1.coef<-reg1$coefficients
summary(reg1)
# R^2 ni bueno ni malo, meh. p valores ok
# No es suficiente...
sum.reg1<-summary(reg1) # guardo summary
sum.reg1$r.squared
reg1$fitted.values

# chequeando lo que sabemos ...
SSE<-sum(reg1$residualsˆ2)
SST<-sum((d2$height-mean(d2$height))^2)
SSM<-sum((reg1$fitted.values-mean(d2$height))^2)
SSM/SST
1-SSE/SST
1-var(reg1$residuals)/var(d2$height)
cor(d2$height,reg1$fitted.values)^2

# SE modelo
df <- dim(d2)[1]-2
SE <- sqrt(SSE/df)
SE

# Predecir a partir del modelo encontrado
new.weights<-data.frame(weight=c(50,62))
predict.lm(object=reg1,newdata=new.weights)
# equivalente a:
reg1.coef[1]+reg1.coef[2]*new.weights[1:2,]

# LINEAL MULTIVARIADO
reg2<-lm(height~weight+age,d) # h=h(w,a), para todos (no rest edad)
summary(reg2)

# podemos verlo
library("scatterplot3d")
s3d <- scatterplot3d(d[,c("weight","age","height")],
                     type="h", highlight.3d=TRUE,
                     angle=55, scale.y=0.7, pch=16,
                     main="height˜weight+age")
s3d$plane3d(reg2, lty.box = "solid")

# REG POLINOMIAL
# INTERESANTE: Normalizar antes de usarla, las potencias pueden hacer explotar cantidades!
# h(w)=b0 + b1*w + b2*w^2
# pb: w toma valores eventualmente altos (>100), w^2 cambia el orden de magnitud!

d$weight_s <-(d$weight-mean(d$weight))/sd(d$weight) # normalizo
reg4 <- lm(height~weight_s+I(weight_s^2),d)
reg4
summary(reg4)
# ploteo
weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
new.weights<-data.frame("weight_s"=weight.seq,
                        "I(weight_s^2)"=weight.seq^2)
h.pred <- predict.lm(object=reg4,newdata=new.weights)
plot( height ~ weight_s , d, col="red" )
lines( weight.seq , h.pred )

# Puedo introducir tb predictores categoricos!
# h(hombre) = b0 + b1*hombre
# donde hombre es dummy (0-1)

d$male<-as.factor(d$male)
reg5<-lm(height~male,d)
reg5$coefficients
summary(reg5)
# notar que, en el fondo tenemos que b0 = h promedio mujer, b1 = dif promedio hombre-mujer en altura.
sum(reg5$coefficients)
means<-tapply(d$height,d$male,mean)
means
means[2]-means[1]
# mismo p valor!
t.test(d$height~d$male, var.equal=T)

# incluir var categ y numeric
reg6<-lm(height~weight+male,d)
reg6

################
# INTERACCIONES
# Idea simple: h(u,v) = b0 + b1*u + b2*v + b3_int*u*v
# Util al mezclar categoricos con numericos
################
# En R se usa * para simbolizar interaccion o :
reg7<-lm(height~weight+male+weight:male,d)
# lm(height˜weight*male,d) tb sirve
reg7

# podemos ver los modelos sin interaccion (en este caso) y notar ciertas similitudes
d.male<-d[d$male==1,]
reg8<-lm(height~weight,d.male)
d.female<-d[d$male==0,]
reg9<-lm(height~weight,d.female)

reg7$coefficients["(Intercept)"]
reg9$coefficient["(Intercept)"]

reg7$coefficients["weight"]
reg9$coefficient["weight"]

# las similitudes vienen de notar que en el modelo con interaccion male=0 => female = 1, etc.
reg7$coefficients["weight:male1"]
reg8$coefficient["weight"]-reg9$coefficient["weight"]

reg7$coefficients["male1"]
reg8$coefficients["(Intercept)"]-reg9$coefficient["(Intercept)"]


