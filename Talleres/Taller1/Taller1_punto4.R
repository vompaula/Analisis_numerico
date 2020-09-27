#     *** Analisis Numerico 2020-3 ***
#--Trabajo realizado por:
# Laura Mariana Jiménez Jiménez
# Paula Valentina Sánchez Peña
# Sebastián Gutiérrez Zambrano
# Taller 1 punto 4: Convergencia acelerada
library(Rmpfr)
library(pracma)
library(Rcpp)

Fx <- function(x) x^2-cos(x)
Gx <- function(x) sqrt(cos(x))

aitken=function(secuencia){
  
  a=secuencia[1]
  b=secuencia[2]
  c=secuencia[3]
  
  x= a -(((b-a)^2)/(a-2*b+c))
  return(x)
}
AitkenMethod <- function(g,x0,tol,n)
{
  errores = c()
  secuencia = c()
  x = 0
  for(i in 1:n)
  {
    secuencia = c(secuencia,as.numeric(x0))
    if(length(secuencia) == 3)
    {
      x0 = mpfr(aitken(secuencia),128)
      secuencia = c()
    }
    x = g(x0)
    error = abs(x0-x)
    errores = c(errores,as.numeric(error))
    if(error <= tol)
    {
      iteraciones <- c(1:i) 
      plot(iteraciones, errores, main = "Medicion del error Aitken", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
      retorno = c(x,i)
      return(retorno)
    }
    x0 <- x
  }
  retorno = c(x,n)
  return(retorno)
}

SteffensenMethod <- function(g,x0,tol,n)
{
  errores = c()
  x1=g(x0)
  x2=g(x1)
  x_nuevo = x0-(x1-x0)^2/(x2-2*x1+x0)
  error= abs(x2-x_nuevo)
  errores = c(errores,as.numeric(error))
  i=1
  while(i<=n & error>=tol)
  {
    x0=x_nuevo
    x1=g(x0)
    x2=g(x1)
    x_nuevo = x0-(x1-x0)^2/(x2-2*x1+x0);
    error= abs(x2-x_nuevo);
    errores = c(errores,as.numeric(error))
    i=i+1;
  }
  iteraciones <- c(1:i-1) 
  plot(iteraciones, errores, main = "Medicion del error Steffensen", xlab= "Iteraciones", ylab = "Errores", type = 'o' ,col="blue")
  retorno = c(x_nuevo,i-1)
  return(retorno)
}
#----------------------------------------------------------------------------------------------
#Método de Steffensen y Método de Aitken para la función f(x) = x^2-cos(x)

#Ambos métodos con una tolerancia de 1e-8
print(AitkenMethod(Gx,5,1e-8,1000))
print(SteffensenMethod(Gx,mpfr(5,128),1e-8,1000))

#Ambos métodos para una tolerancia de 1e-16
print(AitkenMethod(Gx,5,1e-16,1000))
print(SteffensenMethod(Gx,mpfr(5,128),1e-16,1000))

#Comparar con la sucesión A_n con los primeros valores de X_n = cos(1/n)
primeros = cos(1/seq(1,15))
i=1
for (i in 1:length(primeros))
{
  secuencia = c(primeros[i],primeros[i+1],primeros[i+2])
  A_n = aitken(secuencia)
  cat(A_n, "\n")
}

# Coordenadas de donde intersecta f(t) =3sin^3(t) -1 y g(t) = 4sin(t)cos(t)
Ft <- function(t) 3*sin(t)^3 -1
Gt <- function(t) 4*sin(t)*cos(t)
Ht <- function(t) 3*sin(t)^3 -1 - 4*sin(t)*cos(t)

t1<-seq(1,1.5,0.01)
plot(t1,Ft(t1),type="l",col="red", ylim =c(0,2), xlab = "x", ylab="y")
par(new=TRUE)
plot(t1,Gt(t1),type="l",col="blue", ylim= c(0,2),xlab = "x", ylab="y")

raiz = newtonRaphson(Ht,mpfr(1,128),maxiter =1000,tol = 1e-16 )
cat("Las funciones se intersectan en: ")
Coorx = raiz[[1]]
Coory = Ft(Coorx)
Coory2 = Gt(Coorx)
print(Coorx)
print(Coory)
print(Coory2)