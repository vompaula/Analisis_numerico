library(pracma)

exponencial <- function(x) 
{
  e <- exp(1)
  return(e^x)
}

p <- taylor(exponencial, 0, 5)
x <- seq(-1.0, 1.0, length.out=100)
yexponencial <- exponencial(x)
yTaylor <- polyval(p, x)
plot(x, yexponencial, type = "l", main =' Aproximacion de Taylor para e^x', col = "blue", lwd = 3)
#lines(x, yTaylor, col = "red")
#legend('topleft', inset=.05, legend= c("Serie de Taylor", "e^x") 
        #, lwd=c(2.5,2.5), col=c('red', 'blue'), bty='n', cex=.75)