#------------------------------------Inicia Problema 1----------------------------------------#
uno <- as.integer(c(1,rep(0:1,times = 7)))
print(uno)

dos <- as.integer(rep(rep(c(1,0), times =4), times = c(3,1,4,1,5,1,6,1)))
print(dos)

tres <- as.integer(1:13 + c(-1,-3,-2,-6,-3,-9,-4,-12,-5,-15,-6,-18,-7))
print(tres)

cuatro <- as.integer(seq(from = 9, to = 27, by = 3))
print(cuatro)

cinco <- as.integer(c(-19,rep(-19,times = 4)+(seq(from = 2, to = 8, by =2))-(seq(from = 10, to = 40, by =10)),-59))
print(cinco)

seis <- c(2,3)
seisf <- as.integer(c(seis[1],seq(from = 3, to = 7, by = 2),rep(seis[1],times = 5)+c(seq(from = 2, to = 4, by =2),seq(from = 8, to = 12, by =4),4),rep(seis[2],times = 4)+(seq(from = 3, to = 12, by = 3)),seis[2]*7,seis[1]*4,seis[1]*6,seis[1]*10,seis[1]*12+4))
print(seisf)

siete <- as.integer(0:10)
for (x in seq(from = 1, to = 10, by = 2)) {
  siete[x+1] = siete[x+1]*-1
}
print(siete)

ocho <- as.integer(c(7,3,rep(0,times = 9)))
for(x in 3:11){
  ocho[x] = ocho[x-2] + ocho[x-1]
}
print(ocho)
#------------------------Termina Problema 1--------------------------------#

#------------------------Inicia Problema 2--------------------------------#
randomV <- sample(1:100, 20, replace=TRUE)
print(randomV)
sortingA <- function(x){
  n<-length(x)
  for(j in 1:(n-1)){
    for(i in 1:(n-j)){
      if(x[i]>x[i+1]){
        temp<-x[i]
        x[i]<-x[i+1]
        x[i+1]<-temp
      }
    }
  }
  return(x)
}
sortingD <- function(x){
  n<-length(x)
  for(j in 1:(n-1)){
    for(i in 1:(n-j)){
      if(x[i]<x[i+1]){
        temp<-x[i]
        x[i]<-x[i+1]
        x[i+1]<-temp
      }
    }
  }
  return(x)
}
randomVA <- sortingA(randomV)
randomVD <- sortingD(randomV)
unsortV <- sample(randomVD)
cat("Vector aleatorio original: ",randomV)
cat("Vector desordenado: ",unsortV)
cat("Ordenamiento Ascendente: ",randomVA)
cat("Ordenamiento Descendente: ",randomVD)
#------------------------Termina Problema 2--------------------------------#