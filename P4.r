cantidad<-c(100,120,150,170)
tallas<-c("Mediana","Grande","ExtraGrande","Chica")
tapply(cantidad, tallas,mean)
tapply(cantidad, tallas,median)
cat("Stock total: ",sum(cantidad))
#Cambiando etiquetas
fac_1<-factor(levels=c(1,2,3,4),labels=c("Mediana","Grande","ExtraGrande","Chica"))
print(fac_1)
levels(fac_1)<-c("M","G","Ex","S")
print(fac_1)


#----------------Parte2--------------------------------
user <-list (nombre="Josefina de las fuentes", picture=c(url="images/FOTO0001.jpg",width=128,height=128), passport=c(url="images/Drive/Pasaporte0001.jpg",width=43,height=53))
json <-data.frame(id=0101,type="Usuario", usuarios= user, usuarios.car=c(marca="Porsche",submarca="Carrera GT",color="Negro, Celeste"))
print(json)
str(json)
#----------------Parte3--------------------------------
par<-function(n){
  if(n==0){
    return(TRUE)
  }else{
    return(impar(n-1))
  }
}
impar<-function(n){
  if(n==0){
    return(FALSE)
  }else{
    return(par(n-1))
  }
}
impar(3)
par(21)
par(22)

#----------------Parte4--------------------------------
histograma<-function(datos){
  media<-mean(datos)
  mediana<-median(datos)
  dst<-sd(datos)
  hist(datos,xlab="Datos",ylab="Frecuencia")
  abline(v=media,col="#0000ff52") 
  abline(v=mediana,col="#FF33E3")
  abline(v=media+(dst*c(1,-1)),col="gold")
}
datos<-sample(1:2,1725,replace=T)
histograma(datos)

