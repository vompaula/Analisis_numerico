# Reto 3: Modelo Depredador Presa Lotka-Volterra con presas logísticas
#Integrantes: Laura Mariana Jiménez, Paula Valentina Sanchez, Sebastián Gutiérrez
library(deSolve)
LVmod <- function(Tiempo, EstadoInicial, Parametros) {
  with(as.list(c(EstadoInicial, Parametros)), {
    MuertesPorDepredacion    <- beta  * Presas * Depredador
    NatalidadPresas   <- alpha * Presas * (1 - Presas/K)
    MuertesDepredador <- delta * Depredador
    
    dPresas        <- NatalidadPresas - MuertesPorDepredacion
    dDepredador    <- MuertesPorDepredacion * gamma - MuertesDepredador
    
    return(list(c(dPresas, dDepredador)))
  })
}


resolverModelo <- function(metodo,Parametros,yini)
{
  
  Tiempos <- seq(0, 200, by = 1)
  out   <- ode(yini, Tiempos, LVmod, Parametros, method = metodo)
  summary(out)
  
  plot(out[,2],out[,3], main =paste( "Líneas de campo método",metodo),type = "l",xlab = "x",ylab = "y", col = "blue")

  matplot(out[,-1],type = "l",
          xlab = "Tiempo", ylab = "population")
  legend("topright", c("Liebres:presa", "Linces:depredador"),
         lty = c(1,2), col = c(1,2), box.lwd = 0)
  
  return(out)
}

#Prueba 1 de la simulación
Parametros  <- c(beta   = 0.2,    # Tasa de muerte de las presas debido a los depredadores por dias (beta)
                 alpha  = 1.0,    # /Tasa de natalidad de las presas por dias (alpha)
                 delta  = 0.2 ,   # Tasa de mortalidad del depredador por dias
                 gamma = 0.5,    # Tasa de éxito en la caza
                 K      = 50)     # Capacidad de carga / sustentación de la especie
yini  <- c(Presas = 30, Depredador = 4)
outRk4 = resolverModelo("rk4",Parametros,yini)
outAdams = resolverModelo("adams",Parametros, yini)

errorAbsPresas = c()
errorAbsDepre = c()
i = 1
print(outAdams[,2])
while(i < length(outRk4))
{
  errorPresa = abs(outAdams[i,2]-outRk4[i,2])
  errorAbsPresas = c(errorAbsPresas,errorPresa)
  errorDepre = abs(outAdams[i,3]-outRk4[i,3])
  errorAbsDepre= c(errorAbsDepre,errorDepre)
  i = i + 1
}
plot(outRk4[,1],errorAbsPresas, main ="Error absoluto de Liebres",type = "l",xlab = "x",ylab = "y", col = "purple")
plot(outRk4[,1],errorAbsDepre, main ="Error absoluto de Linces",type = "l",xlab = "x",ylab = "y", col = "purple")

#Prueba 2 de la simulación duplicando la tasa de nacimiento de las presas
Parametros  <- c(beta   = 0.2,    # Tasa de muerte de las presas debido a los depredadores por dias (beta)
                 alpha  = 2,    # /Tasa de natalidad de las presas por dias (alpha)
                 delta  = 0.2 ,   # Tasa de mortalidad del depredador por dias
                 gamma = 0.5,    # Tasa de éxito en la caza
                 K      = 50)     # Capacidad de carga / sustentación de la especie
yini  <- c(Presas = 30, Depredador = 4)
outRk4 = resolverModelo("rk4",Parametros,yini)
outAdams = resolverModelo("adams",Parametros, yini)

#Prueba 3 de la simulación aumentando la tasa de mortalidad de las presas de 0.2 a 0.6
Parametros  <- c(beta   = 0.2,    # Tasa de muerte de las presas debido a los depredadores por dias (beta)
                 alpha  = 1.0,    # /Tasa de natalidad de las presas por dias (alpha)
                 delta  = 0.6 ,   # Tasa de mortalidad del depredador por dias
                 gamma = 0.5,    # Tasa de éxito en la caza
                 K      = 50)     # Capacidad de carga / sustentación de la especie
yini  <- c(Presas = 30, Depredador = 4)
outRk4 = resolverModelo("rk4",Parametros,yini)
outAdams =resolverModelo("adams",Parametros, yini)
