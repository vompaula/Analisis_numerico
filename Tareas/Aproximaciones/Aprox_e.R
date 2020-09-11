library(pracma)
options(digits = 8)

x <- 0.01
n <- 5

exponencial <- function(x) 
{
  e <- exp(1)
  return(e^x)
}

p <- taylor(exponencial, 0, n)
t <- polyval(p, x)
#cat(t)
#cat("\n", cos(x))
e <- cos (x) - t
r <- t + e
cat ("Aproximación de Taylor", "->", t)
cat("\nAproximación sumado con el error ->", r)
cat("\ne^",x," -> ", exponencial(x))
cat("\nError ->", e)