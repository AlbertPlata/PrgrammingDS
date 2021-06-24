#-------------------------------Problema1--------------------------------------#
#75% de la población local muy buen agrado
#Jurado 55 personas
#¿Cuál es la probabilidad de que como máximo el 50% del jurado proclame el libro
# como un best seller?
pbinom(q=55/2,size=55,prob=.75)
#¿Cuál sería la cantidad de personas del jurado requeridas para que se tenga una
# probabilidad de 90% para que se proclame el libro como un best seller?


#-------------------------------Problema2--------------------------------------#
#150 GoodYear 210 Michelline 43 muestras azar
#¿Cuál es la probabilidad de que todas las muestras tomadas sean del proveedor GoodYear?
dhyper(x=43, m=150, n=210, k=43)
#¿Cuál es la probabilidad de que 10 o más proveengan del proveedor Michellin?
sum(dhyper(x=10:43, m=210, n=150, k=43))

#-------------------------------Problema3--------------------------------------#
#sobre peso h > m
# promedio_h = 90 kg s^2 = 16 kg
# promedio_m = 80 kg s^2 = 25 kg
#¿Cuál es la probabilidad de que una amiga tuya en ese país tenga un peso igual o mayor a 85 kg?
#P(x>=85) = 1 - P(x<85)
1-pnorm(85, mean=80, sd=25)
#¿Cuál es el peso de un hombre de ese país que se encuentra en el percentil 45?
qnorm(0.45, mean=90, sd=16)

#-------------------------------Problema4--------------------------------------#
#                                 (3.2)                                        #
#determine el intervalo de confianza al 90% y 95%
#media_h = 789       media_m = 1081
#Intervalo al 90% hombre--------------------------------------------------------
numero_muestras<-789
media_muestras<-85
desv<-25
nivelConfianza=.9
error_estimado<-desv/sqrt(numero_muestras)
margen_error<-1.644854*error_estimado
limite_inf<-media_muestras-margen_error
limite_sup<-media_muestras+margen_error
print(limite_inf)
print(limite_sup)

#Intervalo al 95% hombre--------------------------------------------------------
numero_muestras<-789
media_muestras<-85
desv<-25
nivelConfianza=.95
error_estimado<-desv/sqrt(numero_muestras)
margen_error<-1.644854*error_estimado
limite_inf<-media_muestras-margen_error
limite_sup<-media_muestras+margen_error
print(limite_inf)
print(limite_sup)
#Intervalo al 90% mujer---------------------------------------------------------
numero_muestras<-1081
media_muestras<-80
desv<-16
nivelConfianza=.9
error_estimado<-desv/sqrt(numero_muestras)
margen_error<-1.644854*error_estimado
limite_inf<-media_muestras-margen_error
limite_sup<-media_muestras+margen_error
print(limite_inf)
print(limite_sup)
#Intervalo al 95% mujer---------------------------------------------------------
numero_muestras<-1081
media_muestras<-80
desv<-16
nivelConfianza=.95
error_estimado<-desv/sqrt(numero_muestras)
margen_error<-1.644854*error_estimado
limite_inf<-media_muestras-margen_error
limite_sup<-media_muestras+margen_error
print(limite_inf)

#-------------------------------Problema5--------------------------------------#
#Bugs: A = 1, 1, 9, 3, 1, 5, 7, 4, 5, 7   B = 1, 1, 8, 2, 2, 7, 7, 7, 2, 1
#¿Cuál plataforma tiene menores bugs y con que porcentaje de confianza pueden 
# afirmarse los resultados?

x <- c(1, 1, 9, 3, 1, 5, 7, 4, 5, 7) #Plataforma A
y <- c(1, 1, 8, 2, 2, 7, 7, 7, 2, 1) #Plataforma B
t.test(x, y, paired = FALSE,var.equal =FALSE, conf.level = .95)
message("Con un porcentaje del 95% la plataforma A tiene menores Bugs")


