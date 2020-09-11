Fx <- function (x,n,v)
{
  r<-x-(x^(n-v))/(n*x^(n-1))
  return(r)
}
  
Gx <- function (x,n,v)
{
  r<- x^(n-v)
  return(r)
}
 
Hx <- function (x,n)
{
  r<-n*x^(n-1)
  return(r)
}
  
calcularRaiz <- function (v,n)
{
  options(digits = 10)
  x <- v/2
  error <- 1
  i <- 0
  while (error < 1.e-8)
  {
    x <- Fx(x,n,v)
    error <- abs(Gx(x,n,v))/Hx(x,n)
    cat(" x=", x, " \t error= ", error, "\n")
    i <- i+1
  }
}

calcularRaiz(9, 2)