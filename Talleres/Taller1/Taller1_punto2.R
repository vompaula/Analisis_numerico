#     *** Analisis Numerico 2020-3 ***
#--Trabajo realizado por:
# Laura Mariana Jiménez Jiménez
# Paula Valentina Sánchez Peña
# Sebastián Gutiérrez Zambrano
# Taller 1 punto 2: Raíces de una Ecuación
library(Rmpfr)
library(pracma)
library(Matrix)

#Imprimir los elementos de la diagonal superior de una matriz
sumaTriangularSuperior <- function(tam)
{
  res = c()
  for (n in tam) 
  {
    A<-ones(n, m = n)
    S = upper.tri(A)
    res = c(res,sum(S))
  }
  print(res)
  return(res)
}
graficar = sumaTriangularSuperior(seq(2,100,2))

plot(seq(2,100,2),graficar,xlab = "n",ylab = "Suma", type = 'o',col = "red")

#-------------------------------------------------------------------------------------
#Sumar los primeros n^2 números naturales
sumaNumerosNaturales <- function(tam)
{
  num = c()
  res = c()
  for (n in tam) 
  {
    num = c(num,n^2)
    res = c(res,sum(num))
  }
  print(res)
  return(res)
}
graficar = sumaNumerosNaturales(seq(1,100))
plot(seq(1,100),graficar,xlab = "n",ylab = "Suma", type = 'o',col = "blue")

#------------------------------------------------------------------------------------
#Hallar los maxima altura de la función de vuelo de un cohete y(t) = 6+2.13t^2-0.0013t^4

Fx <- function(x) 6+2.13*x^2 -0.0013*x^4 
f <- expression( 6+2.13*x^2 -0.0013*x^4 )

dx = D(f,"x")
fdx <- function(x) eval(dx)

maximo = newtonRaphson(fdx,x0=mpfr(20,128),maxiter = 10000,tol = 1e-8)
alturaMaxima = Fx(maximo[[1]])
cat("Newton-Raphson: La altura maxima que puede alcanzar el cohete (en metros) es de")
print(alturaMaxima)

maximo = bisect(fdx,a=mpfr(20,128),b=mpfr(40,128),maxiter = 1000)
alturaMaxima = Fx(maximo[[1]])
cat("Bisección: La altura maxima que puede alcanzar el cohete (en metros) es de")
print(alturaMaxima)