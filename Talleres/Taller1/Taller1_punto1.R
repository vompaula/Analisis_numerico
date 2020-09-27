#     *** Analisis Numerico 2020-3 ***
#--Trabajo realizado por:
# Laura Mariana Jiménez Jiménez
# Paula Valentina Sánchez Peña
# Sebastián Gutiérrez Zambrano
# Taller 1 punto 1: Número de operaciones
library(Rmpfr)
library(pracma)
library(binaryLogic)
derivada <- function(x) (50*x^{51}-51*x^{50}+1)/(x-1)^{2}
derivada3 <- function(x) -(6*(x^{51}-1))/(x-1)^4 + (306*x^{50})/(x-1)^3 - (7650*x^{49})/(x-1)^2+ (124950*x^{48})/(x-1)


#Algoritmo de Horner
horner <- function(x0,pol){
  res = 0
  for(i in 1:length(pol)){
    res = res*x0 + pol[i]
  }
  return(res)
}
#Método para derivar un polinomio
derivar_Pol <- function(poli){
  derivada = c()
  tam = length(poli)-1
  for (i in 1:length(poli))
  {
    if(tam==0)
      return(derivada)
    derivada <- c(derivada,poli[i]*(tam))
    tam = tam -1
  }
}

x = 1.00000000001
p = c()
for (i in 1:51)
{
  p = c(p,1)
}
# Versión polinómica con horner
dx = derivar_Pol(p)
res = horner(x,dx)
dx2 = derivar_Pol(dx)
dx3 = derivar_Pol(dx2)
res1 = horner(x,dx3)


cat(res)
cat(res1)

#Versión simplificada con (x^{51}-1/x-1)
x = mpfr(x,256)
resp = derivada(x)
print(resp)
resp1 = derivada3(x)
print(resp1)

errorAbs1  =abs(res-resp)
errorRel1 = errorAbs1/resp

errorAbs2 = abs(res1-resp1)
errorRel2 = errorAbs2/resp1

cat(as.numeric(errorAbs1))
cat(as.numeric(errorAbs2))
cat(as.numeric(errorRel1))
cat(as.numeric(errorRel2))

#------------------------------------------------------------------------------------
#Convertir el número pi en binario para los primeros 15 bits
doubletoBinary<-function(numero,precision)
{ numero = round(numero,8)
  entera = as.integer(numero)
  decimal = numero-entera
  
  binEntera = as.binary(entera)
  binDecimal = c()
  aux = decimal
  i=1
  while(i<=precision && aux !=0)
  {
    aux = aux*2
    if(aux >= 1)
    {
      binDecimal = c(binDecimal,1)
      aux = aux -1
    }else{
      binDecimal = c(binDecimal,0)
    }
    i = i +1
  }
  binEntera = paste(binEntera,collapse = '')
  res = c(binEntera,".",binDecimal)
  res = toString(paste(res,collapse = ''))
  return(res)
}

print(doubletoBinary(pi,15))
#-----------------------------------------------------------------------------
#Convertir un número binario a base 10
binarytoDecimal <- function(binario)
{
  bin = strsplit(binario, "\\.")[[1]]
  #Convertir parte entera
  bEnteraS = bin[1]
  bEntera = strsplit(bEnteraS, "")[[1]]
  bE = rev(bEntera)
  entera = 0
  for(i in 1:length(bEntera))
  {
    if(bE[i] =="1")
    entera = entera + 2^{i-1}
    
  }
  if(!length(bin)>1)
  {
    return(entera)
  }
   else
  {
    #Convertir parte fraccional
    bFraccionalS = bin[2]
    bFraccional = strsplit(bFraccionalS, "")[[1]]
    fraccional = 0
    for (i in 1:length(bFraccional))
    {
      fraccional = fraccional + as.numeric(bFraccional[i])*(1/(2^{i}))
    }
    
    numeroDecimal = entera + fraccional
    return(numeroDecimal)
  }

}
cat(binarytoDecimal("1010101"))
cat(binarytoDecimal("1011.101"))
cat(binarytoDecimal("10111.01010101010101010101010101"))
cat(binarytoDecimal("111.111111111111111111111111"))
#-----------------------------------------------------------------------------
#Convertir número decimales a binarios
doubletoBinary(11.25,8)
doubletoBinary((2/3),8)
doubletoBinary(30.6,50)
doubletoBinary(99.9,50)
#-----------------------------------------------------------------------------
#Representación punto flotante IEEE 752 con precisión doble de 0.4
fl = "0"
exponente = (2^{11-1}-1-2)
fl = strcat(fl,"0")
fl = strcat(fl,doubletoBinary(exponente,52))
fl = strRep(fl,".","")
frac = doubletoBinary(0.4,50)
cat(frac)
mantisa = strRep(frac,"0.0","")
mantisa = strcat("100",mantisa)
fl = strcat(fl,mantisa)
fl = strcat(fl,"0")

cat(fl)

#-----------------------------------------------------------------------------
#Encontrar la mayor precisión para la ecuación general en x^2 +9^(12)x = 3

quadratic <- function(a,b,c)
{
  
  
  num = -b -sqrt(b**2 - 4*a*c)
  dem=2*a
  return(num/2)
}


quadratic2<- function(a, b, c)
{
  r = sqrt(b**2 - 4*a*c)
  return ( 2*c/(-b-r))
  
}

x1 <- quadratic(1,9^{12},-3)
x2<- quadratic2(1,9^{12},-3)
cat(x1)
cat(x2)



