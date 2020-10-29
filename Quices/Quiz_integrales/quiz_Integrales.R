#Puntos
#(0.1, 1.8), (0.2, 2.6), (0.3, 3.0), (0.4, 2.8), (0.5, 1.9)

options(digits = 4)
library(plotly)

simpson <- function(x, y, n) {
  
  s = ((x[n]-x[1])/6) * (y[1] + 4*((x[1]+x[n])/2) +  + y[n])
  
  return(s)
}

x = c(0.1, 0.2, 0.3, 0.4, 0.5)
y = c(1.8, 2.6, 3.0, 2.8, 1.9)
n = 5
cat(simpson(x, y, n))
cat("\n")

plot(x,y, type = "l")
