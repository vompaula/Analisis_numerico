#     *** Analisis Numerico 2020-3 ***
#--Trabajo realizado por:
# Laura Mariana Jiménez Jiménez
# Paula Valentina Sánchez Peña
# Sebastián Gutiérrez Zambrano
# Punto 3: Óptima aproximación polinómica

library(PolynomF)
library(capn)
library(pracma)

fx <- function(x) exp(sin(x)-cos(x)^2)

aproxTaylor<- function(f,x0,n)
{
  coefpol = taylor(f,x0,n)
  return (polynom(a=rev(coefpol)))
}
remezMethod <- function(n,a,b,E)
{

  #Nodos de Chebyshev para la aproximación inicial
  nodos = chebnodegen(n+2,-2.e-8,2.e-8)
  
  fxChebyshev = fx(nodos)
  
  matrizEcuaciones = matrix(nrow = n+2,ncol = n+2)
  
  for (i in 1:(n+2)) 
  {
    matrizEcuaciones[i,1] = 1
    
    for (j in 2:(n+1)) 
    {
      matrizEcuaciones[i,j] = nodos[i]^(j-1)
    }
    if(i%%2 == 0)
    {
      matrizEcuaciones[i,n+2] = E
    }
    else
    {
      matrizEcuaciones[i,n+2] = - E
    }
  }
  
  coefPoli =  solve(matrizEcuaciones,fxChebyshev)
  
  #Generación del polinomio a partir de sus coeficientes
  polAprox = polynom(a=coefPoli)
  
  
  return(polAprox)
  
}


n = 1
E = 1.e-4
a = -2.e-8
b = 2.e-8

polAprox = remezMethod(n,a,b,E)
print(polAprox)

polTaylor = aproxTaylor(fx,0,n+1)
print(polTaylor)
x<-seq(a,b,1.e-10)

plot(x,polTaylor(x),type="l",col="red",main = "Polinomio Taylor" )
abline(h=0,col="blue")

plot(x, polAprox(x),type = 'l',col = "blue",main = "Polinomio Remez")
abline(h=0,col="blue")


errorRel= c()
errorAbs = c()
x<-seq(a,b,1.e-8)
errorRel = abs((polTaylor(x)-polAprox(x))/polTaylor(x))
errorAbs = abs((polTaylor(x)-polAprox(x)))
plot(x,errorRel,type = 'l',main = "Error Relativo")
plot(x,errorAbs,type = 'l',main = "Error Absoluto")

  