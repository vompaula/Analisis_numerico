#Esta en la función factorizada, con la cual no hay problema
# de división entre 0
options(digits = 10)

funcion <- function(x)
{
  return ((1-(1-x)^3)/x)
}

#Cuando los numeros son cercanos a 1.5 sus resultados se parecen
cat("F(1.5) = ",funcion(1.5),"\n")
cat("F(1.49) = ",funcion(1.49),"\n")
cat ("F(1.5)-F(1.49) = ",abs((funcion(1.5))-(funcion(1.49))),"\n")

#Con la fórmula anterior no se puede evaluar en 0 ya que no se peude dividir en 0
#Una solución es en vez de usar esa fórmula, usar su equivalente en polinomio
#Este polinomio es equivalente por lo cual resuelve el problema de dividir en 0

plot(funcion, type = "l", col = "blue", lwd = 3)
plot(mipolinomio, type = "l", col = "red", lwd = 3)

#Al imprimir las funciones de puede ver que ambas son iguales

mipolinomio <- function(x)
{
  return ((x^2)-(3*x)+3)
}

cat("F(0) = ",mipolinomio(0))

