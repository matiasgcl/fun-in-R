# Comenzando en R - 1: Fundamentos
# Esencialmente sintáxis y cosas muy básicas

# asignaciones
# evidente
a=10
# alternativas
b <- 1 # convencion usual
assign("tres",3)
d<-a+b
verdad <- T
falsedad <- F

# que clase es una variable dada?
class(verdad)

# como crear funciones?
# asignacion usando <-, se pueden poner valores por defecto en caso de no ser entregados
mi.suma<-function(a=2,b=1){
  return(a+b);
}
mi.suma()
mi.suma(1,1)

# lista de cosas creadas en el workspace? ls
ls()
# liberar una variable?
rm(a)
#liberar todo?
rm(list=ls())

# guardar las variables?
# save.image("archivo.RData")
# cargar? load("archivo.RData")

# asignacion de vectores: c

edades <- c(21,22,37,40,24,14,10)

# añadir atributos 
e.sum<-sum(edades)
e.largo<-length(edades)

# operaciones escalares sobre un vector = aplicada a cada coordenada
edades + 1

# ejemplos: media y varianza
e.media <- sum(edades)/length(edades)
e.var <- sum((edades-e.media)^2)/(length(edades)-1)
# funciones integradas
mean(edades)
var(edades)

# comparemos, verdad si coinciden
print((e.media==mean(edades))&(e.var==var(edades)))

# diferentes tipos en vectores? R fuerza uno común
c("hola",T,1)

# pueden etiquetarse los elementos de un vector, se recuperan via etiqueta al usar NAMES
notas <- c(Juan=4,Pedro=5,Camila=5.5,Diego=3)
names(notas)

# podemos reordenar usando sort
names(sort(x=notas,decreasing=T))

# R indiza desde uno!
notas[1]

# puedo meter vectores para recuperar los que estime conveniente
notas[c(1,3)]

# indice negativo para remover del print
notas[-2]

# acceder por nombre
notas["Juan"]

# Convenciones en operaciones entre vectores
# mismo largo = element wise
# diferente = completacion reciclando el menor (deben ser multiplos, sino tira warning)
a <- c(1,2)
b <- c(3,4)
c <- c(4,5,6)
d <- c(7,8,9,10)
a+b
a+c
a+d

# puedo construir nuevos vectores vía condiciones booleanas
jovenes <- edades<18
edades[jovenes]
# y calcular cosas a partir de esto
mean(edades[jovenes])
mean(edades)
mean(edades[edades>=18]) #otras opciones

# missing data: tipo NA
missing <- c(NA,NA,10,13)
is.na(missing)
missing[is.na(missing)]
missing[!is.na(missing)]

# secuencias de datos
pares <- seq(from=0,to=20,by=2)
todos <- 1:10

# multiples copias
rep(10,3)
rep(c(1,2,3),3)

# Generación datos aleatorios
runif(n=10, min=0, max=1)
rnorm(n=10, mean=500, sd=100)
rpois(n=10, lambda=1)
rbinom(n=10,size=10,prob=0.5) # k = exitos, p = prob, n = n; 

# variables categoricas
gente <-factor(c("H","M","H","M","M","M","H"))
# es posible renombrar
levels(gente) <- c("Man","Woman")

#categorizaciones agregadas
edades<-floor(runif(100,7,44))

categ_edades <- ifelse(edades<12,"niño",ifelse(edades<18,"adolescente","adulto"))
#tipo character
categ_edades <- as.factor(categ_edades)

#usando tapply podemos contar cada tipo o ver cosas que nos interesen
tapply(edades,categ_edades,length)
tapply(edades,categ_edades,mean)
tapply(edades,categ_edades,sd)

# strings y manipulacion
saludo <- "hola"
cat(saludo)

paste("Hola","Chao", sep="-")
paste("Hola gil ",1:4, sep="")

substr(saludo,1,2)

letters

# Matrices, definición y uso

matrix_por_col <- matrix(data=1:12,nrow=3,ncol=4) # por defecto por columna
matrix_por_col

matrix_por_fila <- matrix(data=1:12, nrow=4, ncol=3, byrow=T)
matrix_por_fila

matrix_por_fila[-1,-2] # quito fila 1 col 2

rownames(matrix_por_fila)<-paste("r",1:4,sep="")
colnames(matrix_por_fila)<-paste("c",1:3,sep="")
matrix_por_fila["r2","c3"]

#pegar
rbind(matrix_por_fila,r5=1:3)
cbind(matrix_por_fila,c4=4:1)

# multiplicacion matricial

a <- matrix_por_col %*% matrix_por_fila

# transpuesta
t(a)

# valores y vectores propios
eigen(a)

# tensores
tensor <- array(1:8, dim=c(2,2,2))

# listas
# idea: ser mas flexibles que matrices y tensores (sus miembros pueden tener distintos largos, a diferencia de los últimos)

lista <- list(man="P",woman="J",children=3,ages=c(4,5,6))
lista[c(3,4)]
# para acceder solo al valor (y no key)
lista[[1]]
lista[["man"]]
lista$man # NOTACION MAS USADA

vectors<-list(normal=rnorm(n=100,mean=10,sd=5),
              poisson=rpois(n=100,lambda=10),
              uniform=runif(n=100,min=5,max=15))

# calculo cosas
means <- vector()
desv <- vector()
for(i in 1:length(vectors)){
  means[i] <- mean(vectors[[i]])
  desv[i] <- sd(vectors[[i]])
}

# compactificación: SAPPLY-LAPPLY: aplicar funcion a un vector, retorno vector (SAPPLY), retorno lista (LAPPLY)

sapply(vectors,mean)
sapply(vectors,sd)

lapply(vectors,mean)
lapply(vectors,sd)

# DATA FRAMES: OBJETO MAS COMUN PARA TRABAJAR EN R
# equivalente a tabla de bd

edades.frame <- data.frame(edad=edades,categoria=categ_edades)

#manipulaciones posibles
edades.frame$edad[1:5]
edades.frame[3,1]
edades.frame[3,2]

# guardar como csv
# write.table(x=ages.frame,file="ages.csv",sep=",",row.names=F)
# cargar desde csv
# my.frame<-read.table(file="ages.csv",header=T,sep=",")

# data() para ver datos guardados (test datasets)

# SAMPLING
# EZ AS HELL

sample(edades,size=100,replace=T)
sample(edades,size=100,replace=F)

#a random sample without replacement that has 10 rows from the data.frame USArrests.
USArrests[sample(1:(dim(USArrests)[1]),size=10,replace=F),]

#install.packages("tidyverse") librerías útiles, data.frame -> tibble
library(tidyverse)
edades.tibble <- as_tibble(edades.frame)

#We can select specific rows with the command filter:
edades.tibble %>% filter(edades < 20)

#The command %>% is a pipe: it takes the output from one command and feeds it
#as input to the next command.
#Let’s add a the column using the command bind cols:
weights<-c(60,80,31,70,71,101,59,67,11,78,55,11,
             90,31,65,78,39,35,69,115,63)
edades.tibble <-edades.tibble %>% bind_cols("weights"=weights)
#Now, let’s get all the people who are over 20 years old and weigh over 80
edades.tibble %>% filter(edades > 20 & weights > 80)
