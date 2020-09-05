library(Rmpfr)
rm(list=ls())
options(digits = 16)

Fx <- function(x) cos(2*x)^2 -x^2
expresion1 <- expression (cos(2*x)^2 -x^2)

Fx2 <- function(x) x*sin(x)-1
expresion2 <- expression (x*sin(x)-1)

Fx3 <- function(x) x^3 -2*x^2+(4/3)*x-(8/27)
expresion3 <- expression (x^3 -2*x^2+(4/3)*x-(8/27))

newtonRaphson <- function(a,b,tol,n,f,expresion)
{
  x <- (a+b)/2  
  errores <- c()
  
  for(i in 1:n)
  {
    dx <- eval(D(expresion, "x"))
    if(dx==0)
    {
      cat("Derivada se hace 0 \n")
      res = c(i,x)
      return(res)
    }
    x1 <- x -(f(x)/dx)
    x1 <-mpfr(x1,128)
  
    
    error<-abs(x-x1)
    
    error = as.double(error)
    x= as.double(x)
    errores <- c(errores,error)
    
    cat("I=",i,"\tX=",x,"\tE=",error,"\n")
    
    
    if(error<=tol)
    {
      iteraciones <- c(1:i) 
      plot(iteraciones, errores, main = "Medicion del error Newton Raphson", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
      res = c(i,x1)
      return(res)
    }
    text(x,0,i,cex=0.8,col="blue")
    
    x <- x1
    
  }
}


n = 1000

a = 0
b= 3/2
tol = 1.e-8

x1<-seq(-2,2,0.01)
plot(x1,Fx(x1),type="l",col="red")
abline(h=0,col="blue")

res <- newtonRaphson(a,b,tol,n,Fx,expresion1)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])


a = -1
b= 2
tol = 1.e-16

x1<-seq(-2,2,0.01)
plot(x1,Fx2(x1),type="l",col="red")
abline(h=0,col="blue")

res <- newtonRaphson(a,b,tol,n,Fx2,expresion2)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])

a = 0
b= 2
tol = 1.e-32

x1<-seq(-2,2,0.01)
plot(x1,Fx3(x1),type="l",col="red")
abline(h=0,col="blue")

res <- newtonRaphson(a,b,tol,n,Fx3,expresion3)
iter = res[1]
iter = as.double(iter)
cat("Iteraciones:", iter, "Raiz")
print(res[2])
