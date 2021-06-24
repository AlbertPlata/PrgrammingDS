#------------------------------------Practica Final-----------------------------
#------------------------------------Punto 1------------------------------------
muestra<- sample(1:25,size=100,replace=TRUE)
hist(muestra)
media <- mean(muestra)
sd <- sd(muestra)
abline(v = mean(muestra), col = "blue", lwd = 2)
abline(v = sd(muestra), col = "red", lwd = 2)
decil_8 <- quantile(muestra, c(.8)) 
percentil_5 <- quantile(muestra, c(.05)) 
print(percentil_5)
#Calculando moda
moda <- function(m) {
  uniqv <- unique(m)
  uniqv[which.max(tabulate(match(m, uniqv)))]
}
r <- moda(muestra)
print(r)
#------------------------------------Punto 2------------------------------------
prueba <- shapiro.test(muestra)
print(prueba$p.value)
if(prueba$p.value > 0.05){
  hist(muestra)
}else{
  message("No sigue una distribucion normal")
}
#------------------------------------Punto 3------------------------------------
#Obtengan el intervalo de confianza de las muestras obtenidas del punto uno al 
#95% de confianza.

numero_muestras<-100
media_muestras<- media
desv<-sd
nivelConfianza=.95
error_estimado<-desv/sqrt(numero_muestras)
margen_error<-1.644854*error_estimado
limite_inf<-media_muestras-margen_error
limite_sup<-media_muestras+margen_error
print(limite_inf)
print(limite_sup)

#------------------------------------Punto 4------------------------------------
library(RMySQL)

con <- dbConnect(MySQL(), 
                 user = 'root',
                 password = 'n0m3l0',
                 host = 'localhost',
                 dbname = 'nba')
query <- "SELECT Asistencias_por_partido FROM nba.estadisticas where temporada = '00/01';"
temp_01 <- dbGetQuery(con, query)
query <- "SELECT Asistencias_por_partido FROM nba.estadisticas where temporada = '01/02'LIMIT 128;"
temp_02 <- dbGetQuery(con, query)
t.test(temp_01$Asistencias_por_partido, temp_02$Asistencias_por_partido, paired = TRUE, conf.level = .95)
message("La temporada 1 tuvo un mejor desempeño")
#------------------------------------Punto 5------------------------------------
library(PerformanceAnalytics)

consumo <- c(6, 6.7, 7, 11.1, 6, 6,1, 6.7, 6.5, 7.5)
recorridos <- c(22, 34,32,21,33,21,10, 11, 18, 10)
velocidad <- c(22, 34,32,21,33,21,10, 11, 18, 10)
litros <- c(8, 12, 15, 8, 11, 7, 3, 4, 7, 8)

linear <- function(con,rec,vel,lit){
  lmFirstPair <-lm(rec~vel)
  plot(rec~vel)
  abline(lmFirstPair)
}
linear(consumo,recorridos,velocidad,litros)
#¿Cuál sería el consumo promedio para un recorrido de 110km, 150km y 300 km?
predict(lm(litros~consumo), consumo = 110)
predict(lm(litros~consumo), consumo = 150)
predict(lm(litros~consumo), consumo = 300)
#Utilice el coeficiente de determinación para otorgar una observación del patrón
#de comportamiento de los datos a través de su correlación
cor(consumo,recorridos)
cor(consumo, velocidad)
cor(consumo,litros)
message("Como se puede observar en relacion con la variable consumo, no ay una correlacion significativa con las demas variables")

cor(recorridos,velocidad)
cor(recorridos,litros)
message("Como podemos ver la corelacion más significativa se encuentra entre recorridos y velocidad")

cor(litros,velocidad)
message("Esta ultima correlacion tambien tiene un relacion muy significativa")


#------------------------------------Punto 6------------------------------------
#101 A 99 B 17 muestras azar
#¿Cuál es la probabilidad de que todas las muestras tomadas sean del proveedor A?
dhyper(x=17, m=101, n=99, k=17)
#¿Cuál es la probabilidad de que 15 o más provengan del proveedor B?
sum(dhyper(x=15:17, m=101, n=99, k=17))
#¿Qué cantidad requiero tomar si busco que el 70% de muestras provengan del proveedor A?
qhyper(p=c(0.7),m=101, n=99, k=17)
#Calcule la probabilidad de que como máximo se encuentren 7 acelerómetros del proveedor B.
phyper(q=7, m=101, n=99, k=17)
#------------------------------------Punto 7------------------------------------
#sobre peso h > m
# promedio_h = 90 kg s^2 = 16 kg
# promedio_m = 80 kg s^2 = 25 kg
#¿Cuál es la probabilidad de que una amiga tuya en ese país tenga un peso igual o mayor a 85 kg?
#P(x>=85) = 1 - P(x<85)
1-pnorm(85, mean=80, sd=25)
#¿Cuál es el peso de un hombre de ese país que se encuentra en el percentil 45?
qnorm(0.45, mean=90, sd=16)
#------------------------------------Punto 8------------------------------------
mPoi <- rpois(1000, lambda = 3)
hist(mPoi)
mediaP <- mean(mPoi)
sdP <- sd(mPoi)
#------------------------------------Punto 9------------------------------------

ppois(.02,7)
dpois(0,7)

#------------------------------------Punto 10-----------------------------------
#distribuyen de forma normal en 12 bits   media = 1.1 sd = 1.5
#Calcule la probabilidad de que el sensor genere un valor de 255, 13, 278, 1024 y 0.
dnorm(255, mean=1.1, sd=1.5)
dnorm(13, mean=1.1, sd=1.5)
dnorm(278, mean=1.1, sd=1.5)
dnorm(1024, mean=1.1, sd=1.5)
dnorm(0, mean=1.1, sd=1.5)

