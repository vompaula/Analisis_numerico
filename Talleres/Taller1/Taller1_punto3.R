library(Rmpfr)
library(lamW)
rm(list=ls())
options(digits = 8)

#Primero---------------------------------------------------------------------------------------------------------------------------- 
tol <-1e-8

f <- function(a){
  ln(x + 2) 
}

g <- function(a){
  sin(x)
}

h <- function(x){
  f(x)-g(x)
}

#Gráficas de las funciones
plot(f, xlim = c(-20,20), ylim=c(-20,20), col = "green", main = "green (f(x)) \npurple (g(x)) \n yellow (h(x)) ")
par(new = TRUE)

plot(g, xlim = c(-20,20), ylim=c(-20,20), col = "purple")
par(new = TRUE)

plot(h, xlim = c(-20,20), ylim=c(-20,20), col = "yellow")

#Encontrar el valor teórico
auxiliar <- uniroot(h, c(-1.8,1), tol = 1e-8) 
#De acuerdo con esta función, el valor teorico sería teo
teo <- auxiliar$root

#Ahora el proceso
iteraciones <- c(-5,1,2)
i <- 3

errores <- c(0,0)



while ((abs(iteraciones[i-2]-teo) > tol) & (iteraciones[i-1] > -2))
{
  numerador <- (h(iteraciones[i-1])*(iteraciones[i-1]-iteraciones[i-2]))
  denominador <- h(iteraciones[i-1]) - h(iteraciones[i-2]) 
  if (denominador == 0)
  {
    cat("\n El denominador da 0")
    break
  }
  iteraciones[i] <-iteraciones[i-1]-(numerador/denominador) 
  errores[i] <- abs(teo-iteraciones[i])
  
  i = i+1
  
}

#Se acomoda el contador de iteraciones
i = i - 1

plot(seq(1:i), errores, xlab = "iteraciones", type ="l", main = "errores vs iteraciones\n ecuacion1",col ="red")
cat("\n Resultado", iteraciones[i])
#-----------------------------------------------------------------------------------------------------------

#Ahora el proceso
iteraciones <- c(-1,1,2)
i <- 3

errores <- c(0,0)
while ((abs(iteraciones[i-2]-teo) > tol) & (iteraciones[i-1] > -2))
{
  numerador <- h(iteraciones[i])*(iteraciones[i]-iteraciones[i-1])
  denominador <- h(iteraciones[i]) - h(iteraciones[i-1]) 
  iteraciones[i+1] <- iteraciones[i]-(numerador/denominador) 
  errores[i] <- abs(teo-iteraciones[i])
  i = i+1
}

#Se acomoda el contador de iteraciones
i = i - 1

plot(seq(1:i), errores, xlab = "iteraciones", type ="l", main = "errores vs iteraciones\n ecuacion2",col ="red")
cat("\n Resultado", iteraciones[i])

#-----------------------------------------------------------------------------------------------------------

#Segundo---------------------------------------------------------------------------------------------------------------------------- 

#Primera ecuacion
tol <-1e-8

f <- function(a){
  -a + lambertW0(3-a)
}

g <- function(a){
  -2*a + lambertW0(4 - a)
}

h <- function(x){
  f(x)-g(x)
}

#Gráficas de las funciones
plot(f, xlim = c(-20,20), ylim=c(-20,20), col = "green", main = "green (f(x)) \npurple (g(x)) \n yellow (h(x)) ")
par(new = TRUE)

plot(g, xlim = c(-20,20), ylim=c(-20,20), col = "purple")
par(new = TRUE)

plot(h, xlim = c(-20,20), ylim=c(-20,20), col = "yellow")

#Encontrar el valor teórico
auxiliar <- uniroot(h, c(-1.8,1), tol = 1e-8) 
#De acuerdo con esta función, el valor teorico sería teo
teo <- auxiliar$root

#Ahora el proceso
iteraciones <- c(-5,1,2)
i <- 3

errores <- c(0,0)



while ((abs(iteraciones[i-2]-teo) > tol) & (iteraciones[i-1] > -2))
{
  numerador <- (h(iteraciones[i-1])*(iteraciones[i-1]-iteraciones[i-2]))
  denominador <- h(iteraciones[i-1]) - h(iteraciones[i-2]) 
  if (denominador == 0)
  {
    cat("\n El denominador da 0")
    break
  }
  iteraciones[i] <-iteraciones[i-1]-(numerador/denominador) 
  errores[i] <- abs(teo-iteraciones[i])
  
  i = i+1
  
}

#Se acomoda el contador de iteraciones
i = i - 1

plot(seq(1:i), errores, xlab = "iteraciones", type ="l", main = "errores vs iteraciones\n ecuacion1",col ="red")
cat("\n Resultado", iteraciones[i])
#-----------------------------------------------------------------------------------------------------------
#segunda ecuación
#Ahora el proceso
iteraciones <- c(-1,1,2)
i <- 3

errores <- c(0,0)
while ((abs(iteraciones[i-2]-teo) > tol) & (iteraciones[i-1] > -2))
{
  numerador <- h(iteraciones[i])*(iteraciones[i]-iteraciones[i-1])
  denominador <- h(iteraciones[i]) - h(iteraciones[i-1]) 
  iteraciones[i+1] <- iteraciones[i]-(numerador/denominador) 
  errores[i] <- abs(teo-iteraciones[i])
  i = i+1
}

#Se acomoda el contador de iteraciones
i = i - 1

plot(seq(1:i), errores, xlab = "iteraciones", type ="l", main = "errores vs iteraciones\n ecuacion2",col ="red")
cat("\n Resultado", iteraciones[i])


#-----------------------------------------------------------------------------------------------------------

#Tercero Newton generalizado

Newton = function(Fx, Gx, x0, tol, maxiter)
{
  # Errores actual y anterior
  erroresAnt = c()
  erroresAct = c()
  n = 0
  error = 0
  
  cat(formatC( c(" xn"," f(xn)","Error estimado", "Error anterior"), width = -20, format = "f", flag = " "), "\n")
  
  while(error > tol || n < maxiter )
  {
    #Formula generalizada
    h = Fx(x0)/Gx(x0)
    x1 = x0 - h
    eAnterior = error
    error = abs(x1-x0)
    
    # Imprimir iteraciones
    cat(formatC( c(x1 ,Fx(x1), error,eAnterior), digits=15, width = -15, format = "f", flag = " "), "\n")
    x0 = x1
    n = n+1
    
    if(n>1)
    {
      erroresAnt = c(erroresAnt , eAnterior)
      erroresAct = c(erroresAct, error)
    }
    
  }
  plot(seq(2:n), erroresAnt, main = "Medicion del error Newton generalizado", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
  
  plot.function(Fx, xlim=c(0,0.5), ylim=c(0,1),  main = "Relación error Newton generalizado",xlab = " Error i ",ylab = " Error i+1 ",col ="white")
  
  #imprime la relación de error -> Convergencia lineal
  points(erroresAnt, erroresAct, col = "red")
  lines(erroresAnt, erroresAct, col = "red")
  
  if(n > maxiter)
    
  {
    cat("Se alcanzó el máximo número de iteraciones.\n")
    cat("n = ", n, "Estado: x = ", x1, "Error estimado <= ", h)
  } 
  else 
  {
    cat("Numero iteraciones = ", n, " Raíz = ", x1, " f(x) = ", Fx(x1), " Error estimado = ", error, "\n") 
  }
  
  
}


Fx <- function(x){
  return (exp(1)^x - x - 1 )
}

Gx <- function(x){
  return (exp(1)^x - 1)
}


options(digits = 10)
Newton(Fx,Gx, 2, 1e-8, 29)


#________________________________________________________________________________________________________________________________
#Método de Newton Raphson

newtonRaphson <- function(a,b,tol,n,f, df,expresion)
{
  erroresAnt = c()
  erroresAct = c()
  x <- (a+b)/2  
  errores <- c()
  
  for(i in 1:n)
  {
    x1 <- x -(f(x)/df(x))
    x1 <-mpfr(x1,128)
    
    
    error<-abs(x-x1)
    
    error = as.double(error)
    x= as.double(x)
    errores <- c(errores,error)
    eAnterior = error
    cat("I=",i," X=",x," f(x)=",f(x)," E=",error,"\n")
    
    if(n>1)
    {
      erroresAnt = c(erroresAnt , eAnterior)
      erroresAct = c(erroresAct, error)
    }
    
    if(error<=tol)
    {
      iteraciones <- c(1:i) 
      res = c(i,x1)
      
      #imprime la relación de error -> Convergencia lineal
      points(erroresAnt, erroresAct, col = "red")
      lines(erroresAnt, erroresAct, col = "red")
      
      plot(iteraciones, errores, main = "Medicion del error Newton Raphson", xlab= "Iteraciones", ylab = "Errores", type = 'o' )
      
      return(res)
    }
    x <- x1
  }
  
}

plot.function(Fx, xlim=c(0,0.3), ylim=c(0,1),  main = "Relación error Newton Raphson",xlab = " Error i ",ylab = " Error i+1 ",col ="white")
expresion <- expression (exp(1)^x - x - 1 )
newtonRaphson(1,0,1e-8,29,Fx,Gx,expresion)

plot(Fx, xlim = c(-20,20), ylim=c(-20,20), col = "purple", main = "Gráfica de e^x - x - 1")

