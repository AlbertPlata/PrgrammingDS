#-------------------Parte 1-------------------------#
#Parte 1.1

#𝑥+𝑦+𝑧+𝑤=30 
#2𝑥+2𝑧=20 
#3𝑧+5𝑥+10𝑦+17𝑤=352 
#2𝑦+3𝑧+4𝑤=87

x <- c(1,1,1,1)
y <- c(2,0,2,0)
z <- c(5,10,3,17)
w <- c(0,2,3,4)
A <- rbind(x,y,z,w)

#print(A)

if(det(A) != 0){
  message("El sistema si tiene soluciones calculando...")
  b <- matrix(c(30,20,352,87),byrow = F)
  A_inv <- solve(A)
  resultado  <- A_inv %*% b
  print(resultado)
}else{
  message("El sistema no tiene soluciones")
}
#Parte 2

#2(𝑥+4)/3 −𝑦/2=9/2  =>  12(x+4)-3y=27  =>  12x-3y = -21
#𝑥+2𝑦−(3𝑥−2)/3=−4/3 => 3x+6y-3x-2=-4  =>      6y = -2

x2 <- c(12,-3)
y2 <- c(0,6)
A2 <- rbind(x2,y2)
if(det(A2) != 0){
  message("El sistema si tiene soluciones calculando...")
  b2 <- matrix(c(-21,-2),byrow = F)
  A_inv2 <- solve(A2)
  resultado2  <- A_inv2 %*% b2
  print(resultado2)
}else{
  message("El sistema no tiene soluciones")
}
#Parte 3

#𝑥+3𝑦+𝑧+𝑡=2
#−2𝑥−6𝑦+𝑧−5𝑡=−1
#−𝑥−3𝑦+𝑧−3𝑡=0

x3 <- c(1,3,4,1)
y3 <- c(-2,-6+1-5)
z3 <- c(-1,-3+1-3)
t <- c(0,0,0,0)
A3 <- rbind(x3,y3,z3,t)
if(det(A3) != 0){
  message("El sistema si tiene soluciones calculando...")
  b3 <- matrix(c(2,1,0),byrow = F)
  A_inv3 <- solve(A3)
  resultado3  <- A_inv3 %*% b3
  print(resultado3)
}else{
  message("El sistema no tiene soluciones")
}
#---------------------------Fin Parte 1-------------------#
#------------------Parte 2--------------------------------#
#Parte A y B
sol <- function(matrix1,matrix) {
  if(sqrt(length(matrix1)) == round(sqrt(length(matrix1)))){
    #return("Y")
  }else{
    return("No es un una matriz cuadrada, así que no se puede resolver por Gauss-Jordan")
  }
  
  m <- nrow(matrix)
  n <- ncol(matrix)
  currCol <- 1
  nonZeroRowCount <- 0
  
  while ( (currCol < n+1) & (nonZeroRowCount+1 <= m) ){
    if (sum(matrix[(nonZeroRowCount+1):m, currCol]) == 0){
      currCol <- currCol + 1
    }else{
      rowIndex <- 0
      for (i in nonZeroRowCount+1:m) {
        if (matrix[i,currCol] != 0) {
          rowIndex <- i
          break
        }
      }
      nonZeroRowCount <- nonZeroRowCount + 1

      row1 <- matrix[rowIndex,]
      row2 <- matrix[nonZeroRowCount,]
      matrix[rowIndex,] <- row2
      matrix[nonZeroRowCount,] <- row1
      matrix[nonZeroRowCount,] <- (1/matrix[nonZeroRowCount,currCol]) * matrix[nonZeroRowCount,]
      for (k in 1:m) {
        if ( (matrix[k, currCol] != 0) & (k != nonZeroRowCount) ){
          scalar <- matrix[k, currCol] / matrix[nonZeroRowCount, currCol] 
          matrix[k, ] <- -1 * scalar * matrix[nonZeroRowCount, ] + matrix[k, ]
        }
      }
      currCol <- currCol + 1
    }
  }
  return(matrix)
}

#𝑥−3𝑦+3𝑧=1    ==> |1 -3 3 : 1 |
#3𝑥−9𝑦+2𝑧=10  ==> |3 -9 2 : 10|
v <- c(1,-3,3)
u <-c(3,-9,2)
sis <- rbind(v,u)
sol(sis)

#3𝑥−5𝑦+𝑧=11
#2𝑥−2𝑦+𝑧=7
#𝑥+𝑦+𝑧 = 3
v2 <- c(3,-5,1,11)
u2 <- c(2,-2,1,7)
w2 <- c(1,1,1,3)
v3 <- c(3,-5,1)
u3 <- c(2,-2,1)
w3 <- c(1,1,1)
sis2 <- rbind(v2,u2,w2)
sis3 <- rbind(v3,u3,w3)
sol(sis3,sis2)


#------------------Parte 3--------------------------------#
#Verificar 𝐴M∙𝑋−𝐴M^𝑡∙𝐸=𝐶^2+𝐷/2
# X = [(c^2+d/2)/(AM*E)]+AM^t

f1 <- c(4,1,-2)
f2 <- c(1,2,-1)
f3 <-c(-2,1,0)
AM <- rbind(f1,f2,f3)
f1 <- c(1,2,-2)
f2 <- c(-1,0,2)
f3 <-c(0,2,0)
c <- rbind(f1,f2,f3)
f1 <- c(-6,-12,-2)
f2 <- c(6,-22,-8)
f3 <-c(12,4,-6)
d <- rbind(f1,f2,f3)
f1 <- c(2,0,0)
f2 <- c(0,2,0)
f3 <-c(0,0,2)
e <- rbind(f1,f2,f3)

x <- (((c %*% c)+(d %/% 2))/(AM %*% e)) + t(AM) 
print(x)

#A/B =  A*(1/B)=A*B^-1
#---------------------------Fin Parte 3-------------------#

