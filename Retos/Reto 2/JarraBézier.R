#Reto 2: Elaborar las curvas de nivel de una Jarra con Bezier
#Elaborado por: Laura Mariana Jiménez, Paula Valentina Sanchez, Sebastián Gutiérrez
#Anáñisis Numérico 20-30

library(rgl)
library(bezier)
library(Rmpfr)

VolumenTeorico = mpfr(18369744.3400319539945,64)
VolumenBase = 0
#Primer Nivel
x1 = c(0,36,66,70,78,80)
y1 = c(80,71.442,45.211,38.73,17.776,0)
z1 = 0
VolumenBase = mpfr(pi*x1[6]^2*10,128)
#Segundo Nivel
x2 = c(0,36,66,70,78,80)
y2 = c(80,71.442,45.211,38.73,17.776,0)
z2 = 10
VolumenBase = VolumenBase*2
#Tercer Nivel
x3 = c(0,54,99,105,117,120)
y3 = c(120,107.163,67.816,58.095,26.664,0)
z3 = 20
VolumenBase = VolumenBase + mpfr(pi*x3[6]^2*10,128)
#Cuarto Nivel
x4 = c(0,63,115.5,122.5,136.5,140)
y4 = c(140,125.04,79.119,67.777,31.108,0)
z4 = 30
VolumenBase = VolumenBase + mpfr(pi*x4[6]^2*10,128)
#Quinto Nivel
x5 = c(0,67.5,123.75,131.25,146.25,150)
y5 = c(150,133.954,84.77,72.619,33.33,0)
z5 = 40
VolumenBase = VolumenBase + mpfr(pi*x5[6]^2*10,128)
#Sexto Nivel
x6 = c(0,72,132,140,156,160)
y6 = c(160,142.884,90.422,77.46,35.552,0)
z6 = 50
VolumenBase = VolumenBase + mpfr(pi*x6[6]^2*10,128)
#Septimo Nivel
x7 = c(0,76.5,140.25,148.75,165.75,170)
y7 = c(170,151.815,96.073,82.301,37.774,0)
z7 = 60
VolumenBase = VolumenBase + mpfr(pi*x7[6]^2*10,128)
#Octavo Nivel
x8 = c(0,81,148.5,157.5,175.5,180)
y8 = c(180,160.745,101.724,87.142,39.996,0)
z8 = 70
VolumenBase = VolumenBase + mpfr(pi*x8[6]^2*10,128)
#Noveno Nivel
x9 = c(0,85.5,156.75,166.25,185.25,190)
y9 = c(190,169.175,107.376,91.984,42.218,0)
z9 = 80
VolumenBase = VolumenBase + mpfr(pi*x9[6]^2*10,128)
#Decimo Nivel
x10 = c(0,90,165,175,195,200)
y10 = c(200,178.606,113.027,96.825,44.44,0)
z10 = 90
VolumenBase = VolumenBase + mpfr(pi*x10[6]^2*10,128)
#11 Nivel
x11 = c(0,90,165,175,195,200)
y11 = c(200,178.606,113.027,96.825,44.44,0)
z11 = 100
VolumenBase = VolumenBase + mpfr(pi*x11[6]^2*10,128)
#12 Nivel
x12 = c(0,90,165,175,195,200)
y12 = c(200,178.606,113.027,96.825,44.44,0)
z12 = 110
VolumenBase = VolumenBase + mpfr(pi*x12[6]^2*10,128)
VolumenBase = VolumenBase + mpfr(pi*x9[6]^2*10,128)
VolumenBase = VolumenBase + mpfr(pi*x8[6]^2*10,128)
VolumenBase = VolumenBase + mpfr(pi*x7[6]^2*10,128)
VolumenBase = VolumenBase + mpfr(pi*x6[6]^2*10,128)
VolumenBase = VolumenBase + mpfr(pi*x5[6]^2*10,128)
VolumenBase = VolumenBase + mpfr(pi*x4[6]^2*10,128)
VolumenBase = VolumenBase + mpfr(pi*x3[6]^2*10,128)
#20 Nivel
x20 = c(0,45,82.559,87.5,97.5,100)
y20 = c(100,89.303,56.428,48.412,22.22,0)
z20 = 190
VolumenBase = VolumenBase + mpfr(pi*x20[6]^2*10,128)
#21 Nivel
x21 = c(0,31.5,57.791,61.5,68.5,70)
y21 = c(70,62.512,39.499,33.889,15.554,0)
z21 = 200
VolumenBase = VolumenBase + mpfr(pi*x21[6]^2*10*3,128)

#---------Boquilla-----------
VolumenBoquilla =0
# Zero  nivel
xb0 = c(-56.746,-65,-45.962,0,45.962,65,53.853)
yb0 = c(-31.7,0,45.962,65,45.962,0,-36.398)
xb00 = c(-56.746,-24.236,-14.384,0,13.853,29.263,53.853)
yb00 = c(-31.7,-60.313,-68.506,-75,-68.616,-58.04,-36.398)
zb0 = 230
VolumenBoquilla = mpfr(pi*xb0[6]^2*5,128)
# Primer  nivel
xb1 = c(-61.445,-70,-49.487,0,49.487,70,61.184)
yb1 = c(-33.533,0,49.487,70,49.487,0,-34.008)
xb11 = c(-61.445,-25.952,-7.997,0,5.625,25.448,61.184)
yb11 = c(-33.533,-65.011,-72.724,-70,-73.38,-65.21,-34.008)
zb1 = 235
VolumenBoquilla = VolumenBoquilla + mpfr(pi*xb1[6]^2*5,128)
# Segundo  nivel
xb2 = c(-68.109,-80,-56.569,0,56.569,80,69.394)
yb2 = c(-31.403,0,56.569,80,56.569,0,-28.451)
xb22 = c(-68.109,-43.694,-20.534,0,22.179,46.269,69.394)
yb22 = c(-31.403,-60.958,-72.134,-81,-72.445,-59.027,-28.451)
zb2 = 240
VolumenBoquilla = VolumenBoquilla + mpfr(pi*xb2[6]^2*5,128)
# Tercer nivel
xb3 = c(-79.532,-85,-60.104,0,60.104,85,80.178)
yb3 = c(-29.66,0,60.104,85,60.104,0,-27.865)
xb33 = c(-79.532,-73.51,-31.53,0,28.88,74.445,80.178)
yb33 = c(-29.66,-42.442,-78.809,-88,-79.947,-40.779,-27.865)
zb3 = 245
VolumenBoquilla = VolumenBoquilla + mpfr(pi*xb3[6]^2*5,128)
# Cuarto nivel
xb4 = c(-79.532,-85,-60.104,0,60.104,85,80.178)
yb4 = c(-29.66,0,60.104,85,60.104,0,-27.865)
xb44 = c(-79.532,-57.092,-46.202,0,44.486,62.814,80.178)
yb44 = c(-29.66,-62.814,-71.207,-105,-72.292,-57.092,-27.865)
zb4 = 250
VolumenBoquilla = VolumenBoquilla + mpfr(pi*xb4[6]^2*10,128)
# Quinto nivel
xb5 = c(-79.532,-85,-60.104,0,60.104,85,80.178)
yb5 = c(-29.66,0,60.104,85,60.104,0,-27.865)
xb55 = c(-79.532,-57.092,-21.68,0,23.263,62.814,80.178)
yb55 = c(-29.66,-62.814,-87.849,-95,-86.177,-57.092,-27.865)
zb5 = 255
VolumenBoquilla = VolumenBoquilla + mpfr(pi*xb5[6]^2*10,128)
# Sexto nivel
xb6 = c(-79.532,-85,-60.104,0,60.104,85,80.178)
yb6 = c(-29.66,0,60.104,85,60.104,0,-27.865)
xb66 = c(-79.532,-57.092,-23.885,0,24.403,62.814,80.178)
yb66 = c(-29.66,-62.814,-90.513,-115,-89.653,-57.092,-27.865)
zb6 = 260
VolumenBoquilla = VolumenBoquilla + mpfr(pi*xb6[6]^2*10,128)
# Séptimo nivel
xb7 = c(-81.181,-85,-60.104,0,60.104,85,81.623)
yb7 = c(-31.853,0,60.104,85,60.104,0,-31.209)
xb77 = c(-81.181,-50.682,-18.158,0,15.85,53.638,81.623)
yb77 = c(-31.853,-76.141,-102.104,-108,-103.829,-74.404,-31.209)
zb7 = 265
VolumenBoquilla = VolumenBoquilla + mpfr(pi*xb7[6]^2*10,128)
# Octavo nivel
xb8 = c(-85.916,-90,-63.64,0,63.64,90,86.152)
yb8 = c(-31.853,0,63.64,90,63.64,0,-31.209)
xb88 = c(-85.916,-65.923,-19.828,0,20.165,62.814,86.152)
yb88 = c(-31.853,-60.577,-107.371,-115,-107.026,-69.551,-31.209)
zb8 = 270
VolumenBoquilla = VolumenBoquilla + mpfr(pi*xb8[6]^2*10,128)
# Ultimo nivel
xb9 = c(-94.18,-100,-70.711,0,70.711,100,95.745)
yb9 = c(-33.616,0,70.711,100,70.711,0,-28.859)
xb99 = c(-94.18,-70.8,-23.186,0,29.316,62.814,95.745)
yb99 = c(-33.616,-62.049,-112.072,-121,-106.021,-69.551,-28.859)
zb9 = 275
VolumenBoquilla = VolumenBoquilla + mpfr(pi*xb9[6]^2*10,128)

#--------Manija----------
VolumenManija = 0
#Primer nivel 
xm1 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym1 = c(190,184.142,170,155.858,151,155.858,170,184.142,190)
zm1 = 100
#Segundo Nivel
xm2 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym2 = ym1 +5
zm2 = 110
#Tercer Nivel
xm3 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym3 = ym2 +5
zm3 = 120
#Cuarto Nivel
xm4 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym4 = ym3 +5
zm4 = 130
#Quinto Nivel
xm5 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym5 = ym4 +5
zm5 = 140
#Sexto Nivel
xm6 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym6 = ym5 +5
zm6 = 150
#Septimo Nivel
xm7 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym7 = ym6 +5
zm7 = 160
#Octavo Nivel
xm8 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym8 = ym7 +5
zm8 = 170

#Nivel 15
hManija = 15 *10 #10 es la altura de cada cilindro en cada nivel
rManija = 20 #Hasta el nivel 15 cada cilindro tiene un radio de 20
VolumenManija = mpfr(pi*rManija^2*hManija,128)
#16 Nivel
xm16 = c(0,16.641,20,16.641,0,-16.641,-20,-16.641,0)
ym16 = c(180,166.641,150,133.359,120,133.359,150,166.641,180)
zm16 = 250
VolumenManija = VolumenManija + mpfr(pi*20*30*10,128) #Es un cilindro con radios distintos
#17 Nivel
xm17 = c(0,14.142,20,20.641,0,-19.145,-20,-14.142,0)
ym17 = c(165,159.142,145,16.791,20,16.665,145,159.142,165)
zm17 = 260

rManija = (ym17[1]-ym17[6])/2 #El radio más largo del último cilindro está en el eje y
VolumenManija = VolumenManija + mpfr(pi*20*rManija*5,128)

VolumenJarra = VolumenBase+VolumenBoquilla+VolumenManija
print(VolumenJarra)

errorVolumen = abs(VolumenJarra - VolumenTeorico)/VolumenTeorico
print(errorVolumen)

PlotBase <- function(x,y,z)
{
  x = c(rev(x)*-1,x,rev(x),x*-1)
  y = c(rev(y),y,rev(y)*-1,y*-1)
  t = seq(0,1,length=100)
  
  p <- matrix(c(z,z,z), ncol=3, nrow=24, byrow=TRUE)
  p[,1] = x ; p[,2] = y
  
  curvas_bezier = bezier(t,p)
  
  coorX = curvas_bezier[,1]
  coorY = curvas_bezier[,2]
  coorZ = curvas_bezier[,3]
  
  
  rgl1<-plot3d(coorX,coorY,coorZ,
               type="l",
               size=4,
               add=TRUE,
               xlab="x",ylab="y",zlab="z",
               col = "green")

}

PlotBoquilla <- function(x1,x2,y1,y2,z)
{
  t = seq(0,1,length=100)
  
  p <- matrix(c(z,z,z), ncol=3, nrow=7, byrow=TRUE)
  p[,1] = x1 ; p[,2] = y1
  
  curvas_bezier = bezier(t,p)
  
  coorX = curvas_bezier[,1]
  coorY = curvas_bezier[,2]
  coorZ = curvas_bezier[,3]
  
  rgl1<-plot3d(coorX,coorY,coorZ,
               type="l",
               size=4,
               add=TRUE,
               xlab="x",ylab="y",zlab="z",
               col = "green")
  #Boquilla
  p[,1] = x2 ; p[,2] = y2
  
  curvas_bezier = bezier(t,p)
  
  coorX = curvas_bezier[,1]
  coorY = curvas_bezier[,2]
  coorZ = curvas_bezier[,3]
  
  rgl1<-plot3d(coorX,coorY,coorZ,
               type="l",
               size=4,
               add=TRUE,
               xlab="x",ylab="y",zlab="z",
               col = "green")
  
}

PlotManija <- function(x,y,z)
{
  t = seq(0,1,length=100)
  
  p <- matrix(c(z,z,z), ncol=3, nrow=9, byrow=TRUE)
  p[,1] = x ; p[,2] = y
  
  curvas_bezier = bezier(t,p)
  
  coorX = curvas_bezier[,1]
  coorY = curvas_bezier[,2]
  coorZ = curvas_bezier[,3]
  
  
  rgl1<-plot3d(coorX,coorY,coorZ,
               type="l",
               size=4,
               add=TRUE,
               xlab="x",ylab="y",zlab="z",
               col = "green")
  
}

PlotBase(x1,y1,z1)
PlotBase(x2,y2,z2)
PlotBase(x3,y3,z3)
PlotBase(x4,y4,z4)
PlotBase(x5,y5,z5)
PlotBase(x6,y6,z6)
PlotBase(x7,y7,z7)
PlotBase(x8,y8,z8)
PlotBase(x9,y9,z9)
PlotBase(x10,y10,z10)
PlotBase(x11,y11,z11)
PlotBase(x12,y12,z12)

PlotBase(x9,y9,z12+10)
PlotBase(x8,y8,z12+20)
PlotBase(x7,y7,z12+30)
PlotBase(x6,y6,z12+40)
PlotBase(x5,y5,z12+50)
PlotBase(x4,y4,z12+60)
PlotBase(x3,y3,z12+70)
PlotBase(x20,y20,z20)
PlotBase(x21,y21,z21)
PlotBase(x21,y21,z21+10)
PlotBase(x21,y21,z21+20)

PlotBoquilla(xb0,xb00,yb0,yb00,zb0)
PlotBoquilla(xb1,xb11,yb1,yb11,zb1)
PlotBoquilla(xb2,xb22,yb2,yb22,zb2)
PlotBoquilla(xb3,xb33,yb3,yb33,zb3)
PlotBoquilla(xb4,xb44,yb4,yb44,zb4)
PlotBoquilla(xb5,xb55,yb5,yb55,zb5)
PlotBoquilla(xb6,xb66,yb6,yb66,zb6)
PlotBoquilla(xb7,xb77,yb7,yb77,zb7)
PlotBoquilla(xb8,xb88,yb8,yb88,zb8)
PlotBoquilla(xb9,xb99,yb9,yb99,zb9)

PlotManija(xm1,ym1,zm1)
PlotManija(xm2,ym2,zm2)
PlotManija(xm3,ym3,zm3)
PlotManija(xm4,ym4,zm4)
PlotManija(xm5,ym5,zm5)
PlotManija(xm6,ym6,zm6)
PlotManija(xm7,ym7,zm7)
PlotManija(xm8,ym8,zm8)

PlotManija(xm7,ym7,zm8+10)
PlotManija(xm6,ym6,zm8+20)
PlotManija(xm5,ym5,zm8+30)
PlotManija(xm4,ym4,zm8+40)
PlotManija(xm3,ym3,zm8+50)
PlotManija(xm2,ym2,zm8+60)
PlotManija(xm1,ym1,zm8+70)

PlotManija(xm16,ym16,zm16)
PlotManija(xm17,ym17,zm17)
