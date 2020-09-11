library(pracma)
options(digits = 8)

x<-0.01
n<- 1
# for ( de 1 a 4)
p <- taylor(cos, 0, n)
t <- polyval(p, x)
#fin del for
#cat(t)
#cat("\n", cos(x))
e <- abs(abs(t) - abs(cos (x)))
r <- t + e
cat (p, "\n")
cat ("Aproximación de Taylor", "->", t)
cat("\nAproximación sumado con el error ->", r)
cat("\ncos(",x,") = ", cos(x))
cat("\nError ->", e)





