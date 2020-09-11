#Metodo Horner

polinomio = c(-4,3,-3,0,2)
x = 1


metodoHorner = function(polinomio,x)
{
  
  n_sumas = 0
  n_multpl = 0
  
  resp = polinomio[length(polinomio)]
  
  end = length(polinomio)-1  
  
  for (i in seq(end,1,-1))
  {
    resp = polinomio[i] + resp*x
    
    n_sumas = n_sumas + 1
    n_multpl = n_multpl + 1
    
  }
  
  cat("El polinomio evaluado con el método de Horner es: ",resp, "\n")
  cat("Numero de sumas :",n_sumas,"\n")
  cat("Numero de Multiplicaciones :",n_multpl,"\n \n")
  
  return(resp)
}

metodoHorner(polinomio,x)

#Evaluar con el polinomio P(x)= 1 + x + x^2 + ... + x^50
#Comparar con Q(x) = (x^51 - 1)/ (x-1)

#Evaluar en x=1.0001

polinomio = c()

for (i in 1:51) 
{
  polinomio = c(polinomio,1)
}

P = metodoHorner(polinomio,1.0001)

Q = (((1.0001)^51)-1)/(1.0001-1)

cat("El polinomio evaluado con la expresión equivalente es: ", Q, "\n")
cat("El error absoluto entre P(X) y Q(x) Es: ",abs(P-Q),"\n")
cat("El error relativo entre P(x) y Q(x) Es: ",abs(P-Q)/Q*100,"%\n")

