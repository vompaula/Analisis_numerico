#     *** Analisis Numerico 2020-3 ***
#--Trabajo realizado por:
# Laura Mariana Jiménez Jiménez
# Paula Valentina Sánchez Peña
# Sebastián Gutiérrez Zambrano
# Punto 1: Evaluación de las raíces de un Polinomio: Método Horner, Laguerre, Newton-Horner
library(Rmpfr)
library(pracma)
Fx <- function(x) x^4 -9*x^2 - 5*x^3 + 155*x - 250

#Algoritmo de Horner
horner <- function(x0,pol){
  res = 0
  for(i in 1:length(pol)){
    res = res*x0 + pol[i]
  }
  return(res)
}
#Método para derivar un polinomio
derivar_Pol <- function(poli){
  derivada = c()
  tam = length(poli)-1
  for (i in 1:length(poli))
  {
    if(tam==0)
      return(derivada)
    derivada <- c(derivada,poli[i]*(tam))
    tam = tam -1
  }
}


newtonRaphsonHorner <- function(x,tol,n,poli)
{
  errores <- c()
  
  for(i in 1:n)
  {
    dxPol <- derivar_Pol(poli)
    dx <- horner(x,dxPol)
    if(dx==0)
    {
      cat("Derivada se hace 0 \n")
      res = c(i,x)
      return(res)
    }
    
    Pol_evaluado <- horner(x,poli)
    x1 <- x -(Pol_evaluado/dx)

  
    error<-abs(x-x1)
    
    error = as.double(error)
    x= as.double(x)
    errores <- c(errores,error)
    
    
    cat("I=",i,"\tX=",x,"\tE=",error,"\n")
    
    
    if(error<=tol)
    {
      iteraciones <- c(1:i) 
      plot(iteraciones, errores, main = "Medicion del error Newton Raphson-Horner", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
      res = c(i,x1)
      return(res)
    }
    
    x <- x1
    
  }
}


x1<-seq(-10,10,0.01)
plot(x1,Fx(x1),type="l",col="red")
abline(h=0,col="blue")


#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su primera raiz real con una tolerancia de 1.e-8


n = 1000
x0 <- 3
tol = 1.e-8

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)

#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su segunda raiz real con una tolerancia de 1.e-8
n = 1000

x0 <- -10
tol = 1.e-8

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)

#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su primera raiz imaginaria con una tolerancia de 1.e-8

n = 1000

x0 <- complex(real = 7,imaginary = -5)
tol = 1.e-8

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)

#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su segunda raiz imaginaria con una tolerancia de 1.e-8

n = 1000

x0 <- complex(real = 7,imaginary = 5)
tol = 1.e-8

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)
#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su primera raiz real con una tolerancia de 1.e-16


n = 1000
x0 <- 3
tol = 1.e-16

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)

#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su segunda raiz real con una tolerancia de 1.e-16
n = 1000

x0 <- -10
tol = 1.e-16

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)

#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su primera raiz imaginaria con una tolerancia de 1.e-16

n = 1000

x0 <- complex(real = 7,imaginary = -5)
tol = 1.e-16

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)

#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su segunda raiz imaginaria con una tolerancia de 1.e-16

n = 1000

x0 <- complex(real = 7,imaginary = 5)
tol = 1.e-16

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)
#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su primera raiz real con una tolerancia de 1.e-32


n = 1000
x0 <- 3
tol = 1.e-32

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)

#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su segunda raiz real con una tolerancia de 1.e-32
n = 1000

x0 <- -10
tol = 1.e-32

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)

#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su primera raiz imaginaria con una tolerancia de 1.e-32

n = 1000

x0 <- complex(real = 7,imaginary = -5)
tol = 1.e-32

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)

#---------------------------------------------------------------------------#
# Método de Newton-Horner y método de laguerre para la función x^4 -9*x^2 - 5*x^3 + 155*x - 250
#  para encontrar su segunda raiz imaginaria con una tolerancia de 1.e-32

n = 1000

x0 <- complex(real = 7,imaginary = 5)
tol = 1.e-32

poli = c(1,-5,-9,155,-250)

res <- newtonRaphsonHorner(x0,tol,n,poli)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


laguerre(poli,x0,n,tol)