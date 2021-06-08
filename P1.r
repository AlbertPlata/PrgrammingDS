alumnos <- c(2, 2.5, 7, 8.1, 5.99, 6.01, 8, 1.5, 6.5, 7.1, 9.5, 7.2, 1.9, 2.6, 3.0, 5, 7, 9, 4.1)
vec <- vector()
for (calf in sort(alumnos, decreasing = FALSE)){
  if(calf < 8){
    vec <- c(vec, calf)
  }
}
print(paste0("El indice de alumnos reprobados es: ", (length(vec)*100)/length(alumnos), "%"))
#Punto 2
x <- 3
y <- 3
segundo <- function(y){
  vec2 <- vector()
  for(i in 1:30){
    h <- (y**11)+(y**3) + 1
    y <- ((17**5)*h) %% ((2**21)-1)
    #print(y)
    vec2 <- c(vec2, y)
  }
  return(vec2)
}
primer <- function(x){
  vec1 <- vector()
  for(i in 1:1000){
    g <- (x**3)+(2*x**3) + 1
    x <- ((7**5)*g) %% ((2**32)-1)
    if(round(x/10000)>0 & round(x/10000)<=200){
      vec1 <- c(vec1, round(x/10000))
    }
  }
  return(vec1[0:30])
}
print(primer(x))
