#     *** Analisis Numerico 2020-3 ***
#--Trabajo realizado por:
# Laura Mariana Jiménez Jiménez
# Paula Valentina Sánchez Peña
# Sebastián Gutiérrez Zambrano
# Punto 2: Algoritmo de Brent
library(pracma)
library(mi)
library(Rmpfr)
Fx <- function(x) x^3 -2*x^2+(4/3)*x-(8/27)

brentFun <- function(f,x0,x1,n,tol)
{
  error <- 1
  errores <- c()
  listaErrorAct = c()
  listaErrorAnt = c()
  fx0 <- f(x0)
  fx1 <- f(x1)
  if(fx0 *fx1 >=0)
  {
    return("La función no está entre corchetes")
  }
  
  if(abs(fx0) < abs(fx1))
  { 
    aux1 = x0
    x0 = x1
    x1 = aux1
    
    aux2 = fx0
    fx0 = fx1
    fx1 = aux2
  }
  x2 = x0
  fx2 =fx0
  bandera = TRUE
  i = 0
  d = 0
  
  while(i < n & abs(x1-x0) > tol)
  {
    errorAnterior = error
    
    fx0 = f(x0)
    fx1 = f(x1)
    fx2 = f(x2)
    
    if(fx0 != fx2 & fx1 != fx2)
    {
      op1 = (x0 * fx1 * fx2) / ((fx0 - fx1) *  (fx0 - fx2))
      op2 = (x1 * fx0 * fx2) / ((fx1 - fx0) * (fx1 - fx2))
      op3 = (x2 * fx0 * fx1) / ((fx2 - fx0) * (fx2 - fx1))
      s = op1 + op2 + op3
    }else
    {
      s = x1 - ((fx1 * (x1 - x0))/(fx1-fx0))
    }
    
    if((s < ((3 * x0 + x1)/4)|s>x1)|
       (bandera==TRUE & (abs(s - x1)) >= (abs(x1-x2)/2))|
       (bandera==FALSE & (abs(s-x1)) >= (abs(x2-d)/2))|
       (bandera==TRUE & (abs(x1-x2)) < tol)|
       (bandera==FALSE & (abs(x2-d)) < tol))
    {
      s = (x0 + x1)/2
      bandera = TRUE
    }else
    {
      bandera = FALSE
    }
    fs = f(s)
    d = x2
    x2 = x1
    
    if((fx0 * fs) < 0)
    {
      x1 = s
    }else
    {
      x0 = s
    }
    
    if(abs(fx0)<abs(fx1))
    {
      aux3 = x0
      x0 = x1
      x1 = aux3
    }
    
    error = abs(x1-x0)
    errores = c(errores,as.double(error))
    
    if(i>1)
    {
      listaErrorAnt = c(listaErrorAnt , as.double(errorAnterior))
      listaErrorAct = c(listaErrorAct, as.double(error))
    }
    
    i = i + 1
  }
  
  iteraciones <- c(1:i) 
  plot(iteraciones, errores, main = "Medicion del error Brent", xlab= "Iteraciones", ylab = "Errores", type = 'o', col="blue")
  plot(listaErrorAnt,listaErrorAct, main = "Relación de error Brent",xlab = " Error i ",ylab = " Error i+1 ",type= 'o',col ="red")
  retorno = c(x1,i)
  return(retorno)
}

#---------------------------------------------------------------------------#

x1<-seq(-10,10,0.01)
plot(x1,Fx(x1),type="l",col="red", xlab = "x",ylab = "y")
abline(h=0,col="blue")

# Método de Brent para la función x^3 -2*x^2+(4/3)*x-(8/27)
# con una tolerancia de 1.e-16 con una precisión doble

n=1000
tol = 1.e-16
a = 0.0
b= 5.0

res = brentFun(Fx,a,b,n,tol)
print("Raiz:")
print(res[1],16)
print("Número de iteraciones")
print(res[2])

#---------------------------------------------------------------------------#

# Método de Brent para la función x^3 -2*x^2+(4/3)*x-(8/27)
# con una tolerancia de 1.e-32 con una precisión extendida de 128 bits

n=1000
tol = 1.e-32
a = 0.0
b= 5.0

res = brentFun(Fx,mpfr(a,128),mpfr(b,128),n,tol)
print("Raiz:")
print(res[1])
print("Número de iteraciones")
print(res[2])

#---------------------------------------------------------------------------#

# Método de Brent para la función x^3 -2*x^2+(4/3)*x-(8/27)
# con una tolerancia de 1.e-64 con una precisión extendida de 256 bits

n=1000
tol = 1.e-64
a = 0.0
b= 5.0

res = brentFun(Fx,mpfr(a,256),mpfr(b,256),n,tol)
print("Raiz:")
print(res[1])
print("Número de iteraciones")
print(res[2])
