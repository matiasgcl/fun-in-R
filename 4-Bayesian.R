# 4: Estadistica Bayesiana

# Ejemplo 'lanzamiento tierra': cae en agua o en tierra.

# Grid Approx
# definir grid - buscamos la proporción W-L
p_grid <- seq( from=0 , to=1 , length.out=1000 )
# definir prior - parte uniforme
prior <- rep( 1 , 1000 )
# calcular likelihood en cada valor del grid
likelihood <- dbinom(6, size=9,prob=p_grid)
# calculo de posterior (sin normalizar)
unstd.posterior <- likelihood * prior
# normalizar
posterior <- unstd.posterior / sum(unstd.posterior)
# mirar resultado
plot( p_grid , posterior,type="b",xlab="probability of water",ylab="posterior probability")
mtext("1000 points")

library(rethinking)
# building the model - Laplace
globe.qa <- quap( # quap = quadratic approximation
  alist(
    W ~ dbinom( W+L ,p) , # binomial likelihood
    p ~ dunif(0,1)
    # uniform prior
  ) ,
  data=list(W=24,L=12) ) # jugar con W, L; mejora la media y sd
# display summary of Laplace approximation
precis( globe.qa )

# a la R
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE) # ojo, no es lo mismo que la construccion de posterior, estamos tomando muestra con reemplazo!
dens(samples) # veamos la apariencia de la distribución de esta muestra, density plot

teo.samples<-rbeta(1e4,7,4) # posterior usando beta (teorico!)
dens(teo.samples)
# que tenemos? muestras del grid-approx posterior ~ posterior teorico 
# esto en general no será cierto para modelos más complejos

# cual es la proba posterior que la proporcion de agua es menor o igual a 0.5 ?
sum( posterior[ p_grid < 0.5 ] ) # 0.1718746..., esperado que salga poca considerando los datos.- posterior via grid
pbeta(0.5,7,4) # teorico 0.171875
sum ( samples < 0.5 ) / 1e4 # *muestreo* del posterior via grid, 0.1718

# cuanta posterior proba hay entre 0.5 y 0.75 ?
sum( samples > 0.5 & samples < 0.75 ) / 1e4 # via muestreo 0.5998
pbeta(0.75,7,4)-pbeta(0.5,7,4) # teorico 0.604

# cual es la frontera del 80% menor de la proba posterior?
quantile( samples , 0.8 ) # de muestras 0.763
qbeta(0.8,7,4) # teorico 0.76

# 80% central?
quantile( samples , c( 0.1 , 0.9 ) )
PI( samples , prob=0.8 ) # rethinking
c("10%"=qbeta(0.1,7,4 ),"90%"=qbeta(0.9,7,4 )) #teorico

# EJEMPLO MISLEADING CREDIBLE INTERVAL (exceso de asimetría)
p_grid.a <- seq( from=0 , to=1 , length.out=1000 )
prior.a <- rep(1,1000)
likelihood.a <- dbinom( 3 , size=3 , prob=p_grid.a )
posterior.a <- likelihood.a * prior.a
posterior.a <- posterior.a / sum(posterior.a)
samples.a <- sample( p_grid.a , size=1e4 , replace=TRUE , prob=posterior.a )
dens(samples.a,xlim=c(0,0.935))
PI( samples.a , prob=0.5 ) #deja fuera a p=1, el parametro más probable (W=3, L=0!!)
HPDI( samples.a , prob=0.5 ) # mucho más cerca

PI( samples , prob=0.8 ) # son parecidas en dist no muy skewed
HPDI( samples , prob=0.8 )

# POINT ESTIMATES
# MAP
p_grid[ which.max(posterior.a) ] # via grid aprox
dd <- density(samples.a,adj=0.01) # o via posterior samples
dd$x[which.max(dd$y)]
chainmode( samples.a , adj=0.01 ) # usando rethinking

mean(samples.a) # posterior mean, desde las muestras
median(samples.a)

# cual usar/reportar? TODOS! lo que más ayude a entender la forma del posterior

# Sampling para Simular Prediccion
new_w <- rbinom( 1e5 , size=9 , prob=p_grid[ which.max(posterior) ] ) # usamos likelihood fn (f(d|theta)) con p=MAP para generar nuevas observaciones con 9 lanzamientos
simplehist( new_w , xlab="new water predictions") #veamos la distribucion

# Posterior Predictive Distribution
post_pred_w <- rbinom( 1e4 , size=9 , prob=samples )
simplehist( post_pred_w , xlab="posterior predictions")

# post pred para el lanzamiento de planeta es conocido
# si posterior es beta y likelihood binom => post pred dist es beta-binom
# idea: es binom pero proba de exito en n ensayos es aleatorio como beta
post_pred_w_betabinom <-rbetabinom( n=1e4, size=9, shape1=7, shape2=4)
simplehist( post_pred_w_betabinom , xlab="beta-binomial posterior predictions")

################################
# REGRESION LINEAL BAYESIANA
################################
# partamos de cero
rm(list=ls())
# height: y_i, weight: x_i
# Asumiremos
# y_i ~ Normal(mu_i, sigma), likelihood
# mu_i = b0 + b1 x_i, linear model
# b0 ~ N(150, 50), prior b0
# b1 ~ N(0,1), prior b1
# sigma ~ U(0,50), prior sigma
# 
# al asumir sigma igual para todos, asumimos homocedasticidad 
# los priors son asumidos independientes entre si => f(b0,b1,sigma) = f(b0)*f(b1)*f(sigma) !
#
# justificaciones:
# b0 ~ N(150,50): natural para altura!
# b1 ~ N(0,1): regularizador c/r a overfitting
# sigma ~ U(0,50) evita sd negativas, cota superior da rango razonable para alturas (en el caso extremo)

# Grid aprox ya no es buena idea, 3 params con grid = no es escalable!
# Se usará método de Laplace
# Se obtendran estimaciones de MAP para cada parametro usando un método de optimización (hill-climbing)
# Luego, se ajusta una Normal multivariada centrada en los valores obtenidos.

# Aprox de Laplace: Asumimos que el posterior conjunto satisface:
# f(theta_1,...,theta_m|d) = densidad N(\vec{\mu},Sigma)

# Como obtener \vec{\mu}? MAP!
# Como obtener Sigma? basado en la curvatura (eso expresa después de todo)
# Sigma = [I(\theta_MAP)]^{-1}, I(\theta) = -d_{theta,theta}(log(f(theta|d)))
# como las derivadas son respecto a theta (y en particular de log), no necesitamos calcular la evidencia f(d)! 
# basta con trabajar con el posterior no normalizado f(d|t)*f(t)

# Fitting
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
b.reg1 <- quap(
  alist(
    height ~ dnorm( mu, sigma ),
    mu <- b0 + b1*weight,
    b0 ~ dnorm( 150 , 50 ) ,
    b1 ~ dnorm( 0 , 1) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )

# sumario 
precis( b.reg1, prob=0.95 )
# estos numeros entregan las aprox gaussianas para la distrib marginal posterior de cada parametro
# la marginal de una dist multivariada es la distro univariada de un parametro t_i luego de integrar sobre los otros
# f(t_i|d) = int...int f(t_1,...,t_m|d)dt_1...dt_{i-1}dt_{i+1}...dt_m
# en modelo Gaussiano multivariado, la dist marginal de t_i es una Gaussiana univariada.
# así, la dist marginal de b1 es gaussiana con mu=0.9 y sigma=0.04

# veamos el sumario de la regresion clásica
# Cargar reg1 en Script 3-Inferencia.R
summary(reg1)
# son similares!
# Pq? MLE o estimador de min cuad = MAP con priors uniformes. => Nuestros priors actuales (no unif) no generaron mucho impacto en el posterior

# veamos correlaciones entre parametros (como se relacionan entre ellos en la distribucion de posterior)
cov2cor( vcov( b.reg1 ) )
# b0,b1 correl ~-1, hay que corregir centrando:
d2$weight.c <- d2$weight - mean(d2$weight)
b.reg2 <- quap(
  alist(
    height ~ dnorm( b0 + b1*weight.c, sigma ),
    b0 ~ dnorm( 150 , 50 ) ,
    b1 ~ dnorm( 0 , 1) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )
cov2cor( vcov( b.reg2 ) )
# correl casi cero ;-)

# Ahora: Muestreo desde el posterior
post <- extract.samples( b.reg1, n= 1e4 ) # 10k muestras desde el posterior (con reemplazo)
# veamos como lucen
head(post)
sapply(post,mean) # medias similares a MAP

# Podemos obtener muestras equivalentes usando directamente una normal multivariada (pq usamos aprox Laplace)
library(MASS)
post2 <- mvrnorm( n=1e4,mu=coef(b.reg1),
                    Sigma=vcov(b.reg1 ) )
post2<-as.data.frame(post2)
sapply(post2,mean)
# resultados muy similares

# Ahora, este tipo de cálculo no es suficiente en general para entender la información en la distrib posterior
# ploteemos la inferencia del posterior contra los datos!

# V1: MAP vs data
plot( height~weight , data=d2 , col=rangi2 )
b0_map <- mean(post$b0)
b1_map <- mean(post$b1)
curve( b0_map + b1_map*x, add=TRUE )

# Incluyamos incertitud del posterior, hay muchas cosas aleatorias pasando!
# La idea es construir un intervalo de credibilidad alrededor de la regresión MAP
# Para ello:
# 1) Definir una secuencia de pesos para caclular predicciones
weight.seq <- seq( from=25 , to=70 , by=1 )
# 2) Para cada peso, calculo distrib posterior de la altura promedio mu, usando muestras del posterior:
mu.link <- function(weight) post$b0 + post$b1*weight
# 3) Aplicamos esto a cada peso de la seq
mu <- sapply( weight.seq , mu.link )
# 4) Tenemos un valor MAP de mu para cada peso, via promedio de col de la matriz anterior
mu.mean <- apply( mu , 2 , mean )
# 5) Obtenemos los HPDIs de forma análoga (95% cotas inf-sup para cada peso!)
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.95 )
# 6) ploteamos todos estos intervalos como regiones sombreadas alrededor de la estimación MAP
plot( height ~ weight , data=d2 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 95% HPPDI
shade( mu.HPDI , weight.seq )

# puede usarse
# mu <- link(b.reg1 ,data=data.frame(weight=weight.seq), n=1e4)
# para modelos más complejos, esto generará distrubiciones de posteriores de para mu usando la formula declarada en quap

# INTERVALOS DE PREDICCION

# Idea: generar intervalos de prediccion para alturas simuladas ~y usando la distro posterior predictiva
# lo que recién hicimos fue usar muestras del posterior para visualizar la incertidumbre en mu_i usando el modelo lineal mu_i = b0 + b1*x_i
# las predicciones de altura ~y también dependen de la def estocástica de y en la función de verosim y_i ~ N(mu_i,sigma) !
# la distro Gaussiana de la verosim nos dice que el modelo espera alturas observadas distribuidas alrededor de mu, no directamente sobre este.
# el spread alrededor de mu es gobernado por sigma!
# i.e. hay que meter a sigma de algún modo.

# la distro posterior predictiva usada anteriormente hace eso!
# f(~y|y) = int_t f(~y|t)f(t|y)dt
# en nuestro caso: f(~y|y) = int_b0 int_b1 int_sigma f(~y|b0,b1,sigma)f(b0,b1,sigma|y)db0db1dsigma
# f(~y|b0,b1,sigma) es la verosim de N(b0+b1*~x, sigma^2) y f(b0,b1,sigma|y) es la distro posterior
# se muestrea para evitar calculo integrales.

# Procedimiento:
# Para un peso fijado, muestreo gaussiana con media correcta para ese peso, usando sigma muestreado de la misma distro posterior
# Se hace eso para cada muestra del posterior y para cada peso de interés
# tenemos entonces una colección de alturas simuladas ~y que incluyen la incertidumbre en el posterior y tb la de la verosim Gaussiana
# Primero implementamos una funcion para generar muestra de alturas para un peso dado
# El muestro debe usar la fn de verosim N(b0+b1*w, sigma^2) usando b0,b1,sigma dados por las muestras de posterior
height.weight <- function(weight)
  rnorm(
    n=nrow(post) ,
    mean=post$b0 + post$b1*weight ,
    sd=post$sigma )
sim.height <- sapply( weight.seq , height.weight)
# Nota: la funcion sim del paquete rethinking permite generar facilmente muestras de la distro posterior predictiva de cualquier modelo lineal declarado via quap
# sim.height <- sim( b.reg1 ,n=1e4, data=list(weight=weight.seq))
height.HPDI <- apply( sim.height , 2 , HPDI , prob=0.95 ) # sumarizando
# y ploteamos!
# plot raw data
plot( height ~ weight , d2 , col=col.alpha(rangi2,0.5) )
# draw MAP line
lines( weight.seq , mu.mean )
# draw HPDI region for line
shade( mu.HPDI , weight.seq )
# draw HPDI region for simulated heights
shade( height.HPDI , weight.seq )
# Hay DOS regiones sombreadas: distro de mu por un lado y por otro la region donde el modelo espera tener el 95% de alturas reales, para cada peso.


