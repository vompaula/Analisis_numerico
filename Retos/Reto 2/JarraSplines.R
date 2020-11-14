install.packages("plot3D")

library(rgl)
library(pracma)
library(Rmpfr)

#Imprime en plot 3D
imprimir = function(x,y,z){
  xa = x*-1
  ya = y*-1
  rgl1<-plot3d(x,y,z,
               type="l",
               size=4,
               add=TRUE,
               xlab="x",ylab="y",zlab="z",
               col = "blue")
  rgl1<-plot3d(xa,ya,z,
               type="l",
               size=4,
               add=TRUE,
               xlab="x",ylab="y",zlab="z",
               col = "blue")
  rgl1<-plot3d(xa,y,z,
               type="l",
               size=4,
               add=TRUE,
               xlab="x",ylab="y",zlab="z",
               col = "blue")
  rgl1<-plot3d(x,ya,z,
               type="l",
               size=4,
               add=TRUE,
               xlab="x",ylab="y",zlab="z",
               col = "blue")
}

imprimirm = function(x,y,z){
  rgl1<-plot3d(x,y,z,
               type="l",
               size=4,
               add=TRUE,
               xlab="x",ylab="y",zlab="z",
               col = "blue")
}
#Imprime en 2D puntos los puntos x,y
graficar = function(x,y){
  plot(x,
       y,
       xlab = "X",
       ylab = "Y",
       xlim=c(0,1.3),
       ylim=c(-1.3,1.3))
}

#Calcula el area del poligino
area = function(x,y){
 
  return (polyarea(x, y))
}

#Calcula error por cada spline
error = function(x,y){
  erro = c()
  interpolado = splinefun(x,y)
  for(i in 1:length(x)){
    yExp = interpolado(x)
    err = abs((yExp - y))
    erro = c(erro, err)
  }
  return (erro)
}

volumen = 0
#Primer Nivel
x1 = c(0,36,66,70,78,80)
y1 = c(80,71.442,45.211,38.73,17.776,0)
volumen = area(x1, y1) * 10

y11 = y1[1:3] ; x11 = x1[1:3]
y21 = y1[3:5] ; x21 = x1[3:5]
y31 = y1[5:6] ; x31 = x1[5:6]

vecx = c(x11, x21, x31)
vecy = c(y11, y21, y31)

z1 = 0
graficar(x1, y1)

a11 = spline(x11, y11, n = 201)
a12 = spline(x21, y21, n = 201)
a13 = spline(x31, y31, n = 201)

a111 = a11[1]; a121 = a12[1]; a131 = a13[1]
a111 = unlist(a111, use.names=FALSE); a121 = unlist(a121, use.names=FALSE); a131 = unlist(a131, use.names=FALSE); 
xa1 = c(a111,a121,a131)
a112 = a11[2]; a122 = a12[2]; a132 = a13[2]
a112 = unlist(a112, use.names=FALSE); a122 = unlist(a122, use.names=FALSE); a132 = unlist(a132, use.names=FALSE); 
ya1 = c(a112,a122,a132)

lines(a11, col = "orange4")
lines(a12, col = "orange4")
lines(a13, col = "orange4")

#Segundo Nivel
x2 = c(0,36,66,70,78,80)
y2 = c(80,71.442,45.211,38.73,17.776,0)
volumen = volumen + area(x2,y2) * 10

y12 = y2[1:3] ; x12 = x2[1:3]
y22 = y2[3:5] ; x22 = x2[3:5]
y32 = y2[5:6] ; x32 = x2[5:6]

vecx = c(vecx, x12, x22, x32)
vecy = c(vecy, y12, y22, y32)

z2 = 10
graficar(x2,y2)

a21 = spline(x12, y12, n = 201)
a22 = spline(x22, y22, n = 201)
a23 = spline(x32, y32, n = 201)

a211 = a21[1]; a221 = a22[1]; a231 = a23[1]
a211 = unlist(a211, use.names=FALSE); a221 = unlist(a221, use.names=FALSE); a231 = unlist(a231, use.names=FALSE); 
xa2 = c(a211,a221,a231)
a212 = a21[2]; a222 = a22[2]; a232 = a23[2]
a212 = unlist(a212, use.names=FALSE); a222 = unlist(a222, use.names=FALSE); a232 = unlist(a232, use.names=FALSE); 
ya2 = c(a212,a222,a232)

lines(a21, col = "orange4")
lines(a22, col = "orange4")
lines(a23, col = "orange4")

#Tercer Nivel
x3 = c(0,54,99,105,117,120)
y3 = c(120,107.163,67.816,58.095,26.664,0)
volumen = volumen + area(x3,y3) * 10


y13 = y3[1:3] ; x13 = x3[1:3]
y23 = y3[3:5] ; x23 = x3[3:5]
y33 = y3[5:6] ; x33 = x3[5:6]

vecx = c(vecx, x13, x23, x33)
vecy = c(vecy, y13, y23, y33)

z3 = 20
graficar(x3,y3)

a31 = spline(x13, y13, n = 201)
a32 = spline(x23, y23, n = 201)
a33 = spline(x33, y33, n = 201)

xa3 = c(a31[1], a32[1], a33[1])
ya3 = c(a31[2], a32[2], a33[2])

lines(a31, col = "orange4")
lines(a32, col = "orange4")
lines(a33, col = "orange4")

a311 = a31[1]; a321 = a32[1]; a331 = a33[1]
a311 = unlist(a311, use.names=FALSE); a321 = unlist(a321, use.names=FALSE); a331 = unlist(a331, use.names=FALSE); 
xa3 = c(a311,a321,a331)
a312 = a31[2]; a322 = a32[2]; a332 = a33[2]
a312 = unlist(a312, use.names=FALSE); a322 = unlist(a322, use.names=FALSE); a332 = unlist(a332, use.names=FALSE); 
ya3 = c(a312,a322,a332)

#Cuarto Nivel
x4 = c(0,63,115.5,122.5,136.5,140)
y4 = c(140,125.04,79.119,67.777,31.108,0)
volumen = volumen + area(x4,y4) * 10

y14 = y4[1:3] ; x14 = x4[1:3]
y24 = y4[3:5] ; x24 = x4[3:5]
y34 = y4[5:6] ; x34 = x4[5:6]

vecx = c(vecx, x14, x24, x34, x34)
vecy = c(vecy, y14, y24, y34, x34)

z4 = 30
graficar(x4,y4)

a41 = spline(x14, y14, n = 201)
a42 = spline(x24, y24, n = 201)
a43 = spline(x34, y34, n = 201)

xa4 = c(a41[1], a42[1], a43[1])
ya4 = c(a41[2], a42[2], a43[2])

lines(a41, col = "orange4")
lines(a42, col = "orange4")
lines(a43, col = "orange4")

a411 = a41[1]; a421 = a42[1]; a431 = a43[1]; a431 = a43[1]
a411 = unlist(a411, use.names=FALSE); a421 = unlist(a421, use.names=FALSE); a431 = unlist(a431, use.names=FALSE); a431 = unlist(a431, use.names=FALSE)
xa4 = c(a411,a421,a431,a431)
a412 = a41[2]; a422 = a42[2]; a432 = a43[2]; a432 = a43[2]
a412 = unlist(a412, use.names=FALSE); a422 = unlist(a422, use.names=FALSE); a432 = unlist(a432, use.names=FALSE); a432 = unlist(a432, use.names=FALSE) 
ya4 = c(a412,a422,a432,a432)

#Quinto Nivel
x5 = c(0,67.5,123.75,131.25,146.25,150)
y5 = c(150,133.954,84.77,72.619,33.33,0)
volumen = volumen + area(x5,y5) * 10

y15 = y5[1:3] ; x15 = x5[1:3]
y25 = y5[3:5] ; x25 = x5[3:5]
y35 = y5[5:6] ; x35 = x5[5:6]

vecx = c(vecx, x15, x25, x35, x35)
vecy = c(vecy, y15, y25, y35, y35)

z5 = 40
graficar(x5,y5)

a51 = spline(x15, y15, n = 201)
a52 = spline(x25, y25, n = 201)
a53 = spline(x35, y35, n = 201)

xa5 = c(a51[1], a52[1], a53[1], a53[1])
ya5 = c(a51[2], a52[2], a53[2], a53[2])

lines(a51, col = "orange4")
lines(a52, col = "orange4")
lines(a53, col = "orange4")
lines(a53, col = "orange4")

a511 = a51[1]; a521 = a52[1]; a531 = a53[1]; a531 = a53[1]
a511 = unlist(a511, use.names=FALSE); a521 = unlist(a521, use.names=FALSE); a531 = unlist(a531, use.names=FALSE); a531 = unlist(a531, use.names=FALSE)
xa5 = c(a511,a521,a531,a531)
a512 = a51[2]; a522 = a52[2]; a532 = a53[2]; a532 = a53[2]
a512 = unlist(a512, use.names=FALSE); a522 = unlist(a522, use.names=FALSE); a532 = unlist(a532, use.names=FALSE); a532 = unlist(a532, use.names=FALSE) 
ya5 = c(a512,a522,a532,a532)

#Sexto Nivel
x6 = c(0,72,132,140,156,160)
y6 = c(160,142.884,90.422,77.46,35.552,0)   
volumen = volumen + area(x6,y6) * 10

y16 = y6[1:3] ; x16 = x6[1:3]
y26 = y6[3:5] ; x26 = x6[3:5]
y36 = y6[5:6] ; x36 = x6[5:6]

vecx = c(vecx, x16, x26, x36)
vecy = c(vecy, y16, y26, y36)

z6 = 50
graficar(x6,y6)

a61 = spline(x16, y16, n = 201)
a62 = spline(x26, y26, n = 201)
a63 = spline(x36, y36, n = 201)

xa6 = c(a61[1], a62[1], a62[1])
ya6 = c(a61[2], a62[2], a62[2])

lines(a61, col = "orange4")
lines(a62, col = "orange4")
lines(a63, col = "orange4")

a611 = a61[1]; a621 = a62[1]; a631 = a63[1]
a611 = unlist(a611, use.names=FALSE); a621 = unlist(a621, use.names=FALSE); a631 = unlist(a631, use.names=FALSE); 
xa6 = c(a611,a621,a631)
a612 = a61[2]; a622 = a62[2]; a632 = a63[2]
a612 = unlist(a612, use.names=FALSE); a622 = unlist(a622, use.names=FALSE); a632 = unlist(a632, use.names=FALSE); 
ya6 = c(a612,a622,a632)

#Septimo Nivel
x7 = c(0,76.5,140.25,148.75,165.75,170)
y7 = c(170,151.815,96.073,82.301,37.774,0) 
volumen = volumen + area(x7,y7) * 10

y17 = y7[1:3] ; x17 = x7[1:3]
y27 = y7[3:5] ; x27 = x7[3:5]
y37 = y7[5:6] ; x37 = x7[5:6]

vecx = c(vecx, x17, x27, x37)
vecy = c(vecy, y17, y27, y37)

z7 = 60
graficar(x7,y7)

a71 = spline(x17, y17, n = 201)
a72 = spline(x27, y27, n = 201)
a73 = spline(x37, y37, n = 201)

lines(a71, col = "orange4")
lines(a72, col = "orange4")
lines(a73, col = "orange4")

a711 = a71[1]; a721 = a72[1]; a731 = a73[1]
a711 = unlist(a711, use.names=FALSE); a721 = unlist(a721, use.names=FALSE); a731 = unlist(a731, use.names=FALSE); 
xa7 = c(a711,a721,a731)
a712 = a71[2]; a722 = a72[2]; a732 = a73[2]
a712 = unlist(a712, use.names=FALSE); a722 = unlist(a722, use.names=FALSE); a732 = unlist(a732, use.names=FALSE); 
ya7 = c(a712,a722,a732)

#Octavo Nivel
x8 = c(0,81,148.5,157.5,175.5,180)
y8 = c(180,160.745,101.724,87.142,39.996,0)
volumen = volumen + area(x8,y8) * 10

y18 = y8[1:3] ; x18 = x8[1:3]
y28 = y8[3:5] ; x28 = x8[3:5]
y38 = y8[5:6] ; x38 = x8[5:6]

vecx = c(vecx, x18, x28, x38)
vecy = c(vecy, y18, y28, y38)

z8 = 70
graficar(x8,y8)

a81 = spline(x18, y18, n = 201)
a82 = spline(x28, y28, n = 201)
a83 = spline(x38, y38, n = 201)

lines(a81, col = "orange4")
lines(a82, col = "orange4")
lines(a83, col = "orange4")

a811 = a81[1]; a821 = a82[1]; a831 = a83[1]
a811 = unlist(a811, use.names=FALSE); a821 = unlist(a821, use.names=FALSE); a831 = unlist(a831, use.names=FALSE); 
xa8 = c(a811,a821,a831)
a812 = a81[2]; a822 = a82[2]; a832 = a83[2]
a812 = unlist(a812, use.names=FALSE); a822 = unlist(a822, use.names=FALSE); a832 = unlist(a832, use.names=FALSE); 
ya8 = c(a812,a822,a832)

#Noveno Nivel
x9 = c(0,85.5,156.75,166.25,185.25,190)
y9 = c(190,169.175,107.376,91.984,42.218,0)
volumen = volumen + area(x9,y9) * 10

y19 = y9[1:3] ; x19 = x9[1:3]
y29 = y9[3:5] ; x29 = x9[3:5]
y39 = y9[5:6] ; x39 = x9[5:6]

vecx = c(vecx, x19, x29, x39)
vecy = c(vecy, y19, y29, y39)

z9 = 80
graficar(x9,y9)

a91 = spline(x19, y19, n = 201)
a92 = spline(x29, y29, n = 201)
a93 = spline(x39, y39, n = 201)

lines(spline(x19, y19, n = 201), col = "orange4")
lines(spline(x29, y29, n = 201), col = "orange4")
lines(spline(x39, y39, n = 201), col = "orange4")

a911 = a91[1]; a921 = a92[1]; a931 = a93[1]
a911 = unlist(a911, use.names=FALSE); a921 = unlist(a921, use.names=FALSE); a931 = unlist(a931, use.names=FALSE); 
xa9 = c(a911,a921,a931)
a912 = a91[2]; a922 = a92[2]; a932 = a93[2]
a912 = unlist(a912, use.names=FALSE); a922 = unlist(a922, use.names=FALSE); a932 = unlist(a932, use.names=FALSE); 
ya9 = c(a912,a922,a932)

#Decimo Nivel
x10 = c(0,90,165,175,195,200)
y10 = c(200,178.606,113.027,96.825,44.44,0)
volumen = volumen + area(x10,y10) * 10

y110 = y10[1:3] ; x110 = x10[1:3]
y210 = y10[3:5] ; x210 = x10[3:5]
y310 = y10[5:6] ; x310 = x10[5:6]

vecx = c(vecx, x110, x210, x310)
vecy = c(vecy, y110, y210, y310)

z10 = 90
graficar(x10,y10)

a101 = spline(x110, y110, n = 201)
a102 = spline(x110, y210, n = 201)
a103 = spline(x310, y310, n = 201)

lines(spline(x110, y110, n = 201), col = "orange4")
lines(spline(x210, y210, n = 201), col = "orange4")
lines(spline(x310, y310, n = 201), col = "orange4")

a1011 = a101[1]; a1021 = a102[1]; a1031 = a103[1]
a1011 = unlist(a1011, use.names=FALSE); a1021 = unlist(a1021, use.names=FALSE); a1031 = unlist(a1031, use.names=FALSE); 
xa10 = c(a1011,a1021,a1031)
a1012 = a101[2]; a1022 = a102[2]; a1032 = a103[2]
a1012 = unlist(a1012, use.names=FALSE); a1022 = unlist(a1022, use.names=FALSE); a1032 = unlist(a1032, use.names=FALSE); 
ya10 = c(a1012,a1022,a1032)

#11 Nivel
x11 = c(0,90,165,175,195,200)
y11 = c(200,178.606,113.027,96.825,44.44,0)
volumen = volumen + area(x11,y11) * 10

y111 = y11[1:3] ; x111 = x11[1:3]
y211 = y11[3:5] ; x211 = x11[3:5]
y311 = y11[5:6] ; x311 = x11[5:6]

vecx = c(vecx, x111, x211, x311)
vecy = c(vecy, y111, y211, y311)

z11 = 100
graficar(x11,y11)

a111 = spline(x111, y111, n = 201)
a112 = spline(x211, y211, n = 201)
a113 = spline(x311, y311, n = 201)

lines(spline(x111, y111, n = 201), col = "orange4")
lines(spline(x211, y211, n = 201), col = "orange4")
lines(spline(x311, y311, n = 201), col = "orange4")

a1111 = a111[1]; a1121 = a112[1]; a1131 = a113[1]
a1111 = unlist(a1111, use.names=FALSE); a1121 = unlist(a1121, use.names=FALSE); a1131 = unlist(a1131, use.names=FALSE); 
xa11 = c(a1111,a1121,a1131)
a1112 = a111[2]; a1122 = a112[2]; a1132 = a113[2]
a1112 = unlist(a1112, use.names=FALSE); a1122 = unlist(a1122, use.names=FALSE); a1132 = unlist(a1132, use.names=FALSE); 
ya11 = c(a1112,a1122,a1132)

#12 Nivel
x12 = c(0,90,165,175,195,200)
y12 = c(200,178.606,113.027,96.825,44.44,0)
volumen = volumen + area(x12,y12) * 10

y112 = y12[1:3] ; x112 = x12[1:3]
y212 = y12[3:5] ; x212 = x12[3:5]
y312 = y12[5:6] ; x312 = x12[5:6]

vecx = c(vecx, x112, x212, x312)
vecy = c(vecy, y112, y212, y312)

z12 = 110
graficar(x12,y12)

a121 = spline(x112, y112, n = 201)
a122 = spline(x212, y212, n = 201)
a123 = spline(x312, y312, n = 201)

lines(spline(x112, y112, n = 201), col = "orange4")
lines(spline(x212, y212, n = 201), col = "orange4")
lines(spline(x312, y312, n = 201), col = "orange4")

a1211 = a121[1]; a1221 = a122[1]; a1231 = a123[1]
a1211 = unlist(a1211, use.names=FALSE); a1221 = unlist(a1221, use.names=FALSE); a1231 = unlist(a1231, use.names=FALSE); 
xa12 = c(a1211,a1221,a1231)
a1212 = a121[2]; a1222 = a122[2]; a1232 = a123[2]
a1212 = unlist(a1212, use.names=FALSE); a1222 = unlist(a1222, use.names=FALSE); a1232 = unlist(a1232, use.names=FALSE); 
ya12 = c(a1212,a1222,a1232)


#20 Nivel
x20 = c(0,45,82.559,87.5,97.5,100)
y20 = c(100,89.303,56.428,48.420,22.22,0)
volumen = volumen + area(x20,y20) * 10

y120 = y20[1:3] ; x120 = x20[1:3]
y220 = y20[3:5] ; x220 = x20[3:5]
y320 = y20[5:6] ; x320 = x20[5:6]

vecx = c(vecx, x120, x220, x320)
vecy = c(vecy, y120, y220, y320)

z20 = 190
graficar(x20,y20)

a201 = spline(x120, y120, n = 201)
a202 = spline(x220, y220, n = 201)
a203 = spline(x320, y320, n = 201)

lines(spline(x120, y120, n = 201), col = "orange4")
lines(spline(x220, y220, n = 201), col = "orange4")
lines(spline(x320, y320, n = 201), col = "orange4")

a2011 = a201[1]; a2021 = a202[1]; a2031 = a203[1]
a2011 = unlist(a2011, use.names=FALSE); a2021 = unlist(a2021, use.names=FALSE); a2031 = unlist(a2031, use.names=FALSE); 
xa20 = c(a2011,a2021,a2031)
a2012 = a201[2]; a2022 = a202[2]; a2032 = a203[2]
a2012 = unlist(a2012, use.names=FALSE); a2022 = unlist(a2022, use.names=FALSE); a2032 = unlist(a2032, use.names=FALSE); 
ya20 = c(a2012,a2022,a2032)

#21 Nivel
x21 = c(0,31.5,57.791,61.5,68.5,70)
y21 = c(70,62.521,39.499,33.889,15.554,0)
volumen = volumen + area(x21,y21) * 10

y121 = y21[1:3] ; x121 = x21[1:3]
y221 = y21[3:5] ; x221 = x21[3:5]
y321 = y21[5:6] ; x321 = x21[5:6]

vecx = c(vecx, x121, x221, x321)
vecy = c(vecy, y121, y221, y321)

z21 = 200
graficar(x21,y21)

a211 = spline(x121, y121, n = 201)
a212 = spline(x221, y221, n = 201)
a213 = spline(x321, y321, n = 201)

lines(spline(x121, y121, n = 201), col = "orange4")
lines(spline(x221, y221, n = 201), col = "orange4")
lines(spline(x321, y321, n = 201), col = "orange4")

a2111 = a211[1]; a2121 = a212[1]; a2131 = a93[1]
a911 = unlist(a911, use.names=FALSE); a921 = unlist(a921, use.names=FALSE); a931 = unlist(a931, use.names=FALSE); 
xa9 = c(a911,a921,a931)
a912 = a91[2]; a922 = a92[2]; a932 = a93[2]
a912 = unlist(a912, use.names=FALSE); a922 = unlist(a922, use.names=FALSE); a932 = unlist(a932, use.names=FALSE); 
ya9 = c(a912,a922,a932)

#---------Boquilla-----------
# Zero  nivel
xb0 = c(-56.746,-65,-45.9,0,45.9,65,53.853)
yb0 = c(-31.7,0,45.9,65,45.9,0,-36.39)
volumen = volumen + area(xb0,yb0) * 5

y1b0 = yb0[1:3] ; x1b0 = xb0[1:3]
y2b0 = yb0[3:5] ; x2b0 = xb0[3:5]
y3b0 = yb0[5:7] ; x3b0 = xb0[5:7]

vecx = c(vecx, x1b0, x2b0, x3b0)
vecy = c(vecy, y1b0, y2b0, y3b0)

zb0 = 230
graficar(xb0,yb0)

ab01 = spline(x1b0, y1b0, n = 201)
ab02 = spline(x2b0, y2b0, n = 201)
ab03 = spline(x3b0, y3b0, n = 201)

lines(spline(x1b0, y1b0, n = 201), col = "orange4")
lines(spline(x2b0, y2b0, n = 201), col = "orange4")
lines(spline(x3b0, y3b0, n = 201), col = "orange4")

ab011 = ab01[1]; ab021 = ab02[1]; ab031 = ab03[1]
ab011 = unlist(ab011, use.names=FALSE); ab021 = unlist(ab021, use.names=FALSE); ab031 = unlist(ab031, use.names=FALSE); 
xab0 = c(ab011,ab021,ab031)
ab012 = ab01[2]; ab022 = ab02[2]; ab032 = ab03[2]
ab012 = unlist(ab012, use.names=FALSE); ab022 = unlist(ab022, use.names=FALSE); ab032 = unlist(ab032, use.names=FALSE); 
yab0 = c(ab012,ab022,ab032)

# Primer  nivel
xb1 = c(-61.445,-70,-49.487,0,49.487,70,61.184)
yb1 = c(-33.533,0,49.487,70,49.487,0,-34.008)
volumen = volumen + area(xb1,yb1) * 5

y1b1 = yb1[1:3] ; x1b1 = xb1[1:3]
y2b1 = yb1[3:5] ; x2b1 = xb1[3:5]
y3b1 = yb1[5:7] ; x3b1 = xb1[5:7]

vecx = c(vecx, x1b1, x2b1, x3b1)
vecy = c(vecy, y1b1, y2b1, y3b1)

zb1 = 235
graficar(xb1,yb1)

ab11 = spline(x1b1, y1b1, n = 201)
ab12 = spline(x2b1, y2b1, n = 201)
ab13 = spline(x3b1, y3b1, n = 201)

lines(spline(x1b1, y1b1, n = 201), col = "orange4")
lines(spline(x2b1, y2b1, n = 201), col = "orange4")
lines(spline(x3b1, y3b1, n = 201), col = "orange4")

ab111 = ab11[1]; ab121 = ab12[1]; ab131 = ab13[1]
ab111 = unlist(ab111, use.names=FALSE); ab121 = unlist(ab121, use.names=FALSE); ab131 = unlist(ab131, use.names=FALSE); 
xab1 = c(ab111,ab121,ab131)
ab112 = ab11[2]; ab122 = ab12[2]; ab132 = ab13[2]
ab112 = unlist(ab112, use.names=FALSE); ab122 = unlist(ab122, use.names=FALSE); ab132 = unlist(ab132, use.names=FALSE); 
yab1 = c(ab112,ab122,ab132)

# Segundo  nivel
xb2 = c(-68.109,-80,-56.569,0,56.569,80,69.394)
yb2 = c(-31.403,0,56.569,80,56.569,0,-28.451)
volumen = volumen + area(xb2,yb2) * 5

y1b2 = yb2[1:3] ; x1b2 = xb2[1:3]
y2b2 = yb2[3:5] ; x2b2 = xb2[3:5]
y3b2 = yb2[5:7] ; x3b2 = xb2[5:7]

vecx = c(vecx, x1b2, x2b2, x3b2)
vecy = c(vecy, y1b2, y2b2, y3b2)

zb2 = 240
graficar(xb2,yb2)

ab21 = spline(x1b2, y1b2, n = 201)
ab22 = spline(x2b2, y2b2, n = 201)
ab23 = spline(x3b2, y3b2, n = 201)

lines(spline(x1b2, y1b2, n = 201), col = "orange4")
lines(spline(x2b2, y2b2, n = 201), col = "orange4")
lines(spline(x3b2, y3b2, n = 201), col = "orange4")

ab211 = ab21[1]; ab221 = ab22[1]; ab231 = ab23[1]
ab211 = unlist(ab211, use.names=FALSE); ab221 = unlist(ab221, use.names=FALSE); ab231 = unlist(ab231, use.names=FALSE); 
xab2 = c(ab211,ab221,ab231)
ab212 = ab21[2]; ab222 = ab22[2]; ab232 = ab23[2]
ab212 = unlist(ab212, use.names=FALSE); ab222 = unlist(ab222, use.names=FALSE); ab232 = unlist(ab232, use.names=FALSE); 
yab2 = c(ab212,ab222,ab232)

# Tercer nivel
xb3 = c(-79.532,-85,-60.104,0,60.104,85,80.178)
yb3 = c(-29.66,0,60.104,85,60.104,0,-27.865)
volumen = volumen + area(xb3,yb3) * 5

y1b3 = yb3[1:3] ; x1b3 = xb3[1:3]
y2b3 = yb3[3:5] ; x2b3 = xb3[3:5]
y3b3 = yb3[5:7] ; x3b3 = xb3[5:7]

vecx = c(vecx, x1b3, x2b3, x3b3)
vecy = c(vecy, y1b3, y2b3, y3b3)

zb3 = 245
graficar(xb3,yb3)

ab31 = spline(x1b3, y1b3, n = 201)
ab32 = spline(x2b3, y2b3, n = 201)
ab33 = spline(x3b3, y3b3, n = 201)

lines(spline(x1b3, y1b3, n = 201), col = "orange4")
lines(spline(x2b3, y2b3, n = 201), col = "orange4")
lines(spline(x3b3, y3b3, n = 201), col = "orange4")

ab311 = ab31[1]; ab321 = ab32[1]; ab331 = ab33[1]
ab311 = unlist(ab311, use.names=FALSE); ab321 = unlist(ab321, use.names=FALSE); ab331 = unlist(ab331, use.names=FALSE); 
xab3 = c(ab311,ab321,ab331)
ab312 = ab31[2]; ab322 = ab32[2]; ab332 = ab33[2]
ab312 = unlist(ab312, use.names=FALSE); ab322 = unlist(ab322, use.names=FALSE); ab332 = unlist(ab332, use.names=FALSE); 
yab3 = c(ab312,ab322,ab332)

# Cuarto nivel
xb4 = c(-79.532,-85,-60.104,0,60.104,85,80.178)
yb4 = c(-29.66,0,60.104,85,60.104,0,-27.865)
volumen = volumen + area(xb4,yb4) * 5

y1b4 = yb4[1:3] ; x1b4 = xb4[1:3]
y2b4 = yb4[3:5] ; x2b4 = xb4[3:5]
y3b4 = yb4[5:7] ; x3b4 = xb4[5:7]

vecx = c(vecx, x1b4, x2b4, x3b4)
vecy = c(vecy, y1b4, y2b4, y3b4)

zb4 = 250
graficar(xb4,yb4)

ab41 = spline(x1b4, y1b4, n = 201)
ab42 = spline(x2b4, y2b4, n = 201)
ab43 = spline(x3b4, y3b4, n = 201)

lines(spline(x1b4, y1b4, n = 201), col = "orange4")
lines(spline(x2b4, y2b4, n = 201), col = "orange4")
lines(spline(x3b4, y3b4, n = 201), col = "orange4")

ab411 = ab41[1]; ab421 = ab42[1]; ab431 = ab43[1]
ab411 = unlist(ab411, use.names=FALSE); ab421 = unlist(ab421, use.names=FALSE); ab431 = unlist(ab431, use.names=FALSE); 
xab4 = c(ab411,ab421,ab431)
ab412 = ab41[2]; ab422 = ab42[2]; ab432 = ab43[2]
ab412 = unlist(ab412, use.names=FALSE); ab422 = unlist(ab422, use.names=FALSE); ab432 = unlist(ab432, use.names=FALSE); 
yab4 = c(ab412,ab422,ab432)

# Quinto nivel
xb5 = c(-79.532,-85,-60.104,0,60.104,85,80.178)
yb5 = c(-29.66,0,60.104,85,60.104,0,-27.865)
volumen = volumen + area(xb5,yb5) * 5

y1b5 = yb5[1:3] ; x1b5 = xb5[1:3]
y2b5 = yb5[3:5] ; x2b5 = xb5[3:5]
y3b5 = yb5[5:7] ; x3b5 = xb5[5:7]

vecx = c(vecx, x1b5, x2b5, x3b5)
vecy = c(vecy, y1b5, y2b5, y3b5)

zb5 = 255
graficar(xb5,yb5)

ab51 = spline(x1b5, y1b5, n = 201)
ab52 = spline(x2b5, y2b5, n = 201)
ab53 = spline(x3b5, y3b5, n = 201)

lines(spline(x1b5, y1b5, n = 201), col = "orange4")
lines(spline(x2b5, y2b5, n = 201), col = "orange4")
lines(spline(x3b5, y3b5, n = 201), col = "orange4")

ab511 = ab51[1]; ab521 = ab52[1]; ab531 = ab53[1]
ab511 = unlist(ab511, use.names=FALSE); ab521 = unlist(ab521, use.names=FALSE); ab531 = unlist(ab531, use.names=FALSE); 
xab5 = c(ab511,ab521,ab531)
ab512 = ab51[2]; ab522 = ab52[2]; ab532 = ab53[2]
ab512 = unlist(ab512, use.names=FALSE); ab522 = unlist(ab522, use.names=FALSE); ab532 = unlist(ab532, use.names=FALSE); 
yab5 = c(ab512,ab522,ab532)

# Sexto nivel
xb6 = c(-79.532,-85,-60.104,0,60.104,85,80.178)
yb6 = c(-29.66,0,60.104,85,60.104,0,-27.865)
volumen = volumen + area(xb6,yb6) * 5

y1b6 = yb6[1:3] ; x1b6 = xb6[1:3]
y2b6 = yb6[3:5] ; x2b6 = xb6[3:5]
y3b6 = yb6[5:7] ; x3b6 = xb6[5:7]

vecx = c(vecx, x1b6, x2b6, x3b6)
vecy = c(vecy, y1b6, y2b6, y3b6)

zb6 = 260
graficar(xb6,yb6)

ab61 = spline(x1b6, y1b6, n = 201)
ab62 = spline(x2b6, y2b6, n = 201)
ab63 = spline(x3b6, y3b6, n = 201)

lines(spline(x1b6, y1b6, n = 201), col = "orange4")
lines(spline(x2b6, y2b6, n = 201), col = "orange4")
lines(spline(x3b6, y3b6, n = 201), col = "orange4")

ab611 = ab61[1]; ab621 = ab62[1]; ab631 = ab63[1]
ab611 = unlist(ab611, use.names=FALSE); ab621 = unlist(ab621, use.names=FALSE); ab631 = unlist(ab631, use.names=FALSE); 
xab6 = c(ab611,ab621,ab631)
ab612 = ab61[2]; ab622 = ab62[2]; ab632 = ab63[2]
ab612 = unlist(ab612, use.names=FALSE); ab622 = unlist(ab622, use.names=FALSE); ab632 = unlist(ab632, use.names=FALSE); 
yab6 = c(ab612,ab622,ab632)

# S?ptimo nivel
xb7 = c(-81.181,-85,-60.104,0,60.104,85,81.623)
yb7 = c(-31.853,0,60.104,85,60.104,0,-31.209)
volumen = volumen + area(xb7,yb7) * 5

y1b7 = yb7[1:3] ; x1b7 = xb7[1:3]
y2b7 = yb7[3:5] ; x2b7 = xb7[3:5]
y3b7 = yb7[5:7] ; x3b7 = xb7[5:7]

vecx = c(vecx, x1b7, x2b7, x3b7)
vecy = c(vecy, y1b7, y2b7, y3b7)

zb7 = 265
graficar(xb7,yb7)

ab71 = spline(x1b7, y1b7, n = 201)
ab72 = spline(x2b7, y2b7, n = 201)
ab73 = spline(x3b7, y3b7, n = 201)

lines(spline(x1b7, y1b7, n = 201), col = "orange4")
lines(spline(x2b7, y2b7, n = 201), col = "orange4")
lines(spline(x3b7, y3b7, n = 201), col = "orange4")

ab711 = ab71[1]; ab721 = ab72[1]; ab731 = ab73[1]
ab711 = unlist(ab711, use.names=FALSE); ab721 = unlist(ab721, use.names=FALSE); ab731 = unlist(ab731, use.names=FALSE); 
xab7 = c(ab711,ab721,ab731)
ab712 = ab71[2]; ab722 = ab72[2]; ab732 = ab73[2]
ab712 = unlist(ab712, use.names=FALSE); ab722 = unlist(ab722, use.names=FALSE); ab732 = unlist(ab732, use.names=FALSE); 
yab7 = c(ab712,ab722,ab732)

# Octavo nivel
xb8 = c(-85.916,-90,-63.64,0,63.64,90,86.152)
yb8 = c(-31.853,0,63.64,90,63.64,0,-31.209)
volumen = volumen + area(xb8,yb8) * 5

y1b8 = yb8[1:3] ; x1b8 = xb8[1:3]
y2b8 = yb8[3:5] ; x2b8 = xb8[3:5]
y3b8 = yb8[5:7] ; x3b8 = xb8[5:7]

vecx = c(vecx, x1b8, x2b8, x3b8)
vecy = c(vecy, y1b8, y2b8, y3b8)

zb8 = 270
graficar(xb8,yb8)

ab81 = spline(x1b8, y1b8, n = 201)
ab82 = spline(x2b8, y2b8, n = 201)
ab83 = spline(x3b8, y3b8, n = 201)

lines(spline(x1b8, y1b8, n = 201), col = "orange4")
lines(spline(x2b8, y2b8, n = 201), col = "orange4")
lines(spline(x3b8, y3b8, n = 201), col = "orange4")

ab811 = ab81[1]; ab821 = ab82[1]; ab831 = ab83[1]
ab811 = unlist(ab811, use.names=FALSE); ab821 = unlist(ab821, use.names=FALSE); ab831 = unlist(ab831, use.names=FALSE); 
xab8 = c(ab811,ab821,ab831)
ab8b8 = ab81[2]; ab822 = ab82[2]; ab832 = ab83[2]
ab8b8 = unlist(ab8b8, use.names=FALSE); ab822 = unlist(ab822, use.names=FALSE); ab832 = unlist(ab832, use.names=FALSE); 
yab8 = c(ab8b8,ab822,ab832)

# Ultimo nivel
xb9 = c(-94.18,-100,-70.711,0,70.711,100,95.745)
yb9 = c(-33.616,0,70.711,100,70.711,0,-28.859)
volumen = volumen + area(xb9,yb9) * 5

y1b9 = yb9[1:3] ; x1b9 = xb9[1:3]
y2b9 = yb9[3:5] ; x2b9 = xb9[3:5]
y3b9 = yb9[5:7] ; x3b9 = xb9[5:7]

vecx = c(vecx, x1b9, x2b9, x3b9)
vecy = c(vecy, y1b9, y2b9, y3b9)

zb9 = 275
graficar(xb9,yb9)

ab91 = spline(x1b9, y1b9, n = 201)
ab92 = spline(x2b9, y2b9, n = 201)
ab93 = spline(x3b9, y3b9, n = 201)

lines(spline(x1b9, y1b9, n = 201), col = "orange4")
lines(spline(x2b9, y2b9, n = 201), col = "orange4")
lines(spline(x3b9, y3b9, n = 201), col = "orange4")

ab911 = ab91[1]; ab921 = ab92[1]; ab931 = ab93[1]
ab911 = unlist(ab911, use.names=FALSE); ab921 = unlist(ab921, use.names=FALSE); ab931 = unlist(ab931, use.names=FALSE); 
xab9 = c(ab911,ab921,ab931)
ab912 = ab91[2]; ab922 = ab92[2]; ab932 = ab93[2]
ab912 = unlist(ab912, use.names=FALSE); ab922 = unlist(ab922, use.names=FALSE); ab932 = unlist(ab932, use.names=FALSE); 
yab9 = c(ab912,ab922,ab932)

#--------Manija----------
#Primer nivel 
xm1 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym1 = c(190,184.142,170,155.858,151,155.858,170,184.142,190)
volumen = volumen + area(xm1,ym1) * 10

y1m1 = ym1[1:4] ; x1m1 = xm1[1:4]
y2m1 = ym1[4:7] ; x2m1 = xm1[4:7]
y3m1 = ym1[7:9] ; x3m1 = xm1[7:9]

vecx = c(vecx, x1m1, x2m1, x3m1)
vecy = c(vecy, y1m1, y2m1, y3m1)

zm1 = 100
graficar(xm1,ym1)

am11 = spline(x1m1, y1m1, n = 201)
am12 = spline(x2m1, y2m1, n = 201)
am13 = spline(x3m1, y3m1, n = 201)

lines(spline(x1m1, y1m1, n = 201), col = "orange4")
lines(spline(x2m1, y2m1, n = 201), col = "orange4")
lines(spline(x3m1, y3m1, n = 201), col = "orange4")

am111 = am11[1]; am121 = am12[1]; am131 = am13[1]
am111 = unlist(am111, use.names=FALSE); am121 = unlist(am121, use.names=FALSE); am131 = unlist(am131, use.names=FALSE); 
xam1 = c(am111,am121,am131)
am112 = am11[2]; am122 = am12[2]; am132 = am13[2]
am112 = unlist(am112, use.names=FALSE); am122 = unlist(am122, use.names=FALSE); am132 = unlist(am132, use.names=FALSE); 
yam1 = c(am112,am122,am132)

#Segundo Nivel
xm2 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym2 = ym1 +5
volumen = volumen + area(xm2,ym2) * 10

y1m2 = ym2[1:4] ; x1m2 = xm2[1:4]
y2m2 = ym2[4:7] ; x2m2 = xm2[4:7]
y3m2 = ym2[7:9] ; x3m2 = xm2[7:9]

vecx = c(vecx, x1m2, x2m2, x3m2)
vecy = c(vecy, y1m2, y2m2, y3m2)

zm2 = 110
graficar(xm2,ym2)

am21 = spline(x1m2, y1m2, n = 201)
am22 = spline(x2m2, y2m2, n = 201)
am23 = spline(x3m2, y3m2, n = 201)

lines(spline(x1m2, y1m2, n = 201), col = "orange4")
lines(spline(x2m2, y2m2, n = 201), col = "orange4")
lines(spline(x3m2, y3m2, n = 201), col = "orange4")

am211 = am21[1]; am221 = am22[1]; am231 = am23[1]
am211 = unlist(am211, use.names=FALSE); am221 = unlist(am221, use.names=FALSE); am231 = unlist(am231, use.names=FALSE); 
xam2 = c(am211,am221,am231)
am212 = am21[2]; am222 = am22[2]; am232 = am23[2]
am212 = unlist(am212, use.names=FALSE); am222 = unlist(am222, use.names=FALSE); am232 = unlist(am232, use.names=FALSE); 
yam2 = c(am212,am222,am232)

#Tercer Nivel
xm3 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym3 = ym2 +5
volumen = volumen + area(xm3,ym3) * 10

y1m3 = ym3[1:4] ; x1m3 = xm3[1:4]
y2m3 = ym3[4:7] ; x2m3 = xm3[4:7]
y3m3 = ym3[7:9] ; x3m3 = xm3[7:9]

vecx = c(vecx, x1m3, x2m3, x3m3)
vecy = c(vecy, y1m3, y2m3, y3m3)

zm3 = 120
graficar(xm3,ym3)

am31 = spline(x1m3, y1m3, n = 201)
am32 = spline(x2m3, y2m3, n = 201)
am33 = spline(x3m3, y3m3, n = 201)

lines(spline(x1m3, y1m3, n = 201), col = "orange4")
lines(spline(x2m3, y2m3, n = 201), col = "orange4")
lines(spline(x3m3, y3m3, n = 201), col = "orange4")

am311 = am31[1]; am321 = am32[1]; am331 = am33[1]
am311 = unlist(am311, use.names=FALSE); am321 = unlist(am321, use.names=FALSE); am331 = unlist(am331, use.names=FALSE); 
xam3 = c(am311,am321,am331)
am312 = am31[2]; am322 = am32[2]; am332 = am33[2]
am312 = unlist(am312, use.names=FALSE); am322 = unlist(am322, use.names=FALSE); am332 = unlist(am332, use.names=FALSE); 
yam3 = c(am312,am322,am332)

#Cuarto Nivel
xm4 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym4 = ym3 +5
volumen = volumen + area(xm4,ym4) * 10

y1m4 = ym4[1:4] ; x1m4 = xm4[1:4]
y2m4 = ym4[4:7] ; x2m4 = xm4[4:7]
y3m4 = ym4[7:9] ; x3m4 = xm4[7:9]

vecx = c(vecx, x1m4, x2m4, x3m4)
vecy = c(vecy, y1m4, y2m4, y3m4)

zm4 = 130
graficar(xm4,ym4)

am41 = spline(x1m4, y1m4, n = 201)
am42 = spline(x2m4, y2m4, n = 201)
am43 = spline(x3m4, y3m4, n = 201)

lines(spline(x1m4, y1m4, n = 201), col = "orange4")
lines(spline(x2m4, y2m4, n = 201), col = "orange4")
lines(spline(x3m4, y3m4, n = 201), col = "orange4")

am411 = am41[1]; am421 = am42[1]; am431 = am43[1]
am411 = unlist(am411, use.names=FALSE); am421 = unlist(am421, use.names=FALSE); am431 = unlist(am431, use.names=FALSE); 
xam4 = c(am411,am421,am431)
am412 = am41[2]; am422 = am42[2]; am432 = am43[2]
am412 = unlist(am412, use.names=FALSE); am422 = unlist(am422, use.names=FALSE); am432 = unlist(am432, use.names=FALSE); 
yam4 = c(am412,am422,am432)

#Quinto Nivel
xm5 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym5 = ym4 +5
volumen = volumen + area(xm5,ym5) * 10

y1m5 = ym5[1:4] ; x1m5 = xm5[1:4]
y2m5 = ym5[4:7] ; x2m5 = xm5[4:7]
y3m5 = ym5[7:9] ; x3m5 = xm5[7:9]

vecx = c(vecx, x1m5, x2m5, x3m5)
vecy = c(vecy, y1m5, y2m5, y3m5)

zm5 = 140
graficar(xm5,ym5)

am51 = spline(x1m5, y1m5, n = 201)
am52 = spline(x2m5, y2m5, n = 201)
am53 = spline(x3m5, y3m5, n = 201)

lines(spline(x1m5, y1m5, n = 201), col = "orange4")
lines(spline(x2m5, y2m5, n = 201), col = "orange4")
lines(spline(x3m5, y3m5, n = 201), col = "orange4")

am511 = am51[1]; am521 = am52[1]; am531 = am53[1]
am511 = unlist(am511, use.names=FALSE); am521 = unlist(am521, use.names=FALSE); am531 = unlist(am531, use.names=FALSE); 
xam5 = c(am511,am521,am531)
am512 = am51[2]; am522 = am52[2]; am532 = am53[2]
am512 = unlist(am512, use.names=FALSE); am522 = unlist(am522, use.names=FALSE); am532 = unlist(am532, use.names=FALSE); 
yam5 = c(am512,am522,am532)

#Sexto Nivel
xm6 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym6 = ym5 +5
volumen = volumen + area(xm6,ym6) * 10

y1m6 = ym6[1:4] ; x1m6 = xm6[1:4]
y2m6 = ym6[4:7] ; x2m6 = xm6[4:7]
y3m6 = ym6[7:9] ; x3m6 = xm6[7:9]

vecx = c(vecx, x1m6, x2m6, x3m6)
vecy = c(vecy, y1m6, y2m6, y3m6)

zm6 = 150
graficar(xm6,ym6)

am61 = spline(x1m6, y1m6, n = 201)
am62 = spline(x2m6, y2m6, n = 201)
am63 = spline(x3m6, y3m6, n = 201)

lines(spline(x1m6, y1m6, n = 201), col = "orange4")
lines(spline(x2m6, y2m6, n = 201), col = "orange4")
lines(spline(x3m6, y3m6, n = 201), col = "orange4")

am611 = am61[1]; am621 = am62[1]; am631 = am63[1]
am611 = unlist(am611, use.names=FALSE); am621 = unlist(am621, use.names=FALSE); am631 = unlist(am631, use.names=FALSE); 
xam6 = c(am611,am621,am631)
am612 = am61[2]; am622 = am62[2]; am632 = am63[2]
am612 = unlist(am612, use.names=FALSE); am622 = unlist(am622, use.names=FALSE); am632 = unlist(am632, use.names=FALSE); 
yam6 = c(am612,am622,am632)

#Septimo Nivel
xm7 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym7 = ym6 +5
volumen = volumen + area(xm7,ym7) * 10

y1m7 = ym7[1:4] ; x1m7 = xm7[1:4]
y2m7 = ym7[4:7] ; x2m7 = xm7[4:7]
y3m7 = ym7[7:9] ; x3m7 = xm7[7:9]

vecx = c(vecx, x1m7, x2m7, x3m7)
vecy = c(vecy, y1m7, y2m7, y3m7)

zm7 = 160
graficar(xm7,ym7)

am71 = spline(x1m7, y1m7, n = 201)
am72 = spline(x2m7, y2m7, n = 201)
am73 = spline(x3m7, y3m7, n = 201)

lines(spline(x1m7, y1m7, n = 201), col = "orange4")
lines(spline(x2m7, y2m7, n = 201), col = "orange4")
lines(spline(x3m7, y3m7, n = 201), col = "orange4")

am711 = am71[1]; am721 = am72[1]; am731 = am73[1]
am711 = unlist(am711, use.names=FALSE); am721 = unlist(am721, use.names=FALSE); am731 = unlist(am731, use.names=FALSE); 
xam7 = c(am711,am721,am731)
am712 = am71[2]; am722 = am72[2]; am732 = am73[2]
am712 = unlist(am712, use.names=FALSE); am722 = unlist(am722, use.names=FALSE); am732 = unlist(am732, use.names=FALSE); 
yam7 = c(am712,am722,am732)

#Octavo Nivel
xm8 = c(0,14.142,20,14.142,0,-14.142,-20,-14.142,0)
ym8 = ym7 +5

volumen = volumen + area(xm8,ym8) * 10
y1m8 = ym8[1:4] ; x1m8 = xm8[1:4]
y2m8 = ym8[4:7] ; x2m8 = xm8[4:7]
y3m8 = ym8[7:9] ; x3m8 = xm8[7:9]

vecx = c(vecx, x1m8, x2m8, x3m8)
vecy = c(vecy, y1m8, y2m8, y3m8)

zm8 = 170
graficar(xm8,ym8)

am81 = spline(x1m8, y1m8, n = 201)
am82 = spline(x2m8, y2m8, n = 201)
am83 = spline(x3m8, y3m8, n = 201)

lines(spline(x1m8, y1m8, n = 201), col = "orange4")
lines(spline(x2m8, y2m8, n = 201), col = "orange4")
lines(spline(x3m8, y3m8, n = 201), col = "orange4")

am811 = am81[1]; am821 = am82[1]; am831 = am83[1]
am811 = unlist(am811, use.names=FALSE); am821 = unlist(am821, use.names=FALSE); am831 = unlist(am831, use.names=FALSE); 
xam8 = c(am811,am821,am831)
am812 = am81[2]; am822 = am82[2]; am832 = am83[2]
am812 = unlist(am812, use.names=FALSE); am822 = unlist(am822, use.names=FALSE); am832 = unlist(am832, use.names=FALSE); 
yam8 = c(am812,am822,am832)

#m16 Nivel
xm16 = c(0,16.641,20,16.641,0,-16.641,-20,-16.641,0)
ym16 = c(180,166.641,150,133.359,120,133.359,150,166.641,180)
volumen = volumen + area(x12,y12) * 10

y1m16 = ym16[1:4] ; x1m16 = xm16[1:4]
y2m16 = ym16[4:7] ; x2m16 = xm16[4:7]
y3m16 = ym16[7:9] ; x3m16 = xm16[7:9]

vecx = c(vecx, x1m16, x2m16, x3m16)
vecy = c(vecy, y1m16, y2m16, y3m16)

zm16 = 250
graficar(xm16,ym16)

am161 = spline(x1m16, y1m16, n = 201)
am162 = spline(x2m16, y2m16, n = 201)
am163 = spline(x3m16, y3m16, n = 201)

lines(spline(x1m16, y1m16, n = 201), col = "orange4")
lines(spline(x2m16, y2m16, n = 201), col = "orange4")
lines(spline(x3m16, y3m16, n = 201), col = "orange4")

am1611 = am161[1]; am1621 = am162[1]; am1631 = am163[1]
am1611 = unlist(am1611, use.names=FALSE); am1621 = unlist(am1621, use.names=FALSE); am1631 = unlist(am1631, use.names=FALSE); 
xam16 = c(am1611,am1621,am1631)
am1612 = am161[2]; am1622 = am162[2]; am1632 = am163[2]
am1612 = unlist(am1612, use.names=FALSE); am1622 = unlist(am1622, use.names=FALSE); am1632 = unlist(am1632, use.names=FALSE); 
yam16 = c(am1612,am1622,am1632)

#17 Nivel
xm17 = c(0,14.142,20,20.641,0,-19.145,-20,-14.142,0)
ym17 = c(165,159.142,145,16.791,20,16.665,145,159.142,165)
volumen = volumen + area(xm17,ym17) * 10

y1m17 = ym17[1:4] ; x1m17 = xm17[1:4]
y2m17 = ym17[4:7] ; x2m17 = xm17[4:7]
y3m17 = ym17[7:9] ; x3m17 = xm17[7:9]

vecx = c(vecx, x1m17, x2m17, x3m17)
vecy = c(vecy, y1m17, y2m17, y3m17)

zm17 = 260
graficar(xm17,ym17)

am171 = spline(x1m17, y1m17, n = 201)
am172 = spline(x2m17, y2m17, n = 201)
am173 = spline(x3m17, y3m17, n = 201)

lines(spline(x1m17, y1m17, n = 201), col = "orange4")
lines(spline(x2m17, y2m17, n = 201), col = "orange4")
lines(spline(x3m17, y3m17, n = 201), col = "orange4")

am1711 = am171[1]; am1721 = am172[1]; am1731 = am173[1]
am1711 = unlist(am1711, use.names=FALSE); am1721 = unlist(am1721, use.names=FALSE); am1731 = unlist(am1731, use.names=FALSE); 
xam17 = c(am1711,am1721,am1731)
am1712 = am171[2]; am1722 = am172[2]; am1732 = am173[2]
am1712 = unlist(am1712, use.names=FALSE); am1722 = unlist(am1722, use.names=FALSE); am1732 = unlist(am1732, use.names=FALSE); 
yam17 = c(am1712,am1722,am1732)



imprimir(x1,y1,z1)
imprimir(x2,y2,z2)
imprimir(x3,y3,z3)
imprimir(x4,y4,z4)
imprimir(x5,y5,z5)
imprimir(x6,y6,z6)
imprimir(x7,y7,z7)
imprimir(x8,y8,z8)
imprimir(x9,y9,z9)
imprimir(x10,y10,z10)
imprimir(x11,y11,z11)
imprimir(x12,y12,z12)

imprimir(x9,y9,z12+10)
imprimir(x8,y8,z12+20)
imprimir(x7,y7,z12+30)
imprimir(x6,y6,z12+40)
imprimir(x5,y5,z12+50)
imprimir(x4,y4,z12+60)
imprimir(x3,y3,z12+70)
imprimir(x20,y20,z20)
imprimir(x21,y21,z21)
imprimir(x21,y21,z21+10)
imprimir(x21,y21,z21+20)


#Boquillb
imprimir(xb0,yb0,zb0)
imprimir(xb1,yb1,zb1)
imprimir(xb2,yb2,zb2)
imprimir(xb3,yb3,zb3)
imprimir(xb4,yb4,zb4)
imprimir(xb5,yb5,zb5)
imprimir(xb6,yb6,zb6)
imprimir(xb7,yb7,zb7)
imprimir(xb8,yb8,zb8)
imprimir(xb9,yb9,zb9)

#Manija
imprimirm(xm1,ym1,zm1)
imprimirm(xm2,ym2,zm2)
imprimirm(xm3,ym3,zm3)
imprimirm(xm4,ym4,zm4)
imprimirm(xm5,ym5,zm5)
imprimirm(xm6,ym6,zm6)
imprimirm(xm7,ym7,zm7)
imprimirm(xm8,ym8,zm8)

imprimirm(xm7,ym7,zm8+10)
imprimirm(xm6,ym6,zm8+20)
imprimirm(xm5,ym5,zm8+30)
imprimirm(xm4,ym4,zm8+40)
imprimirm(xm3,ym3,zm8+50)
imprimirm(xm2,ym2,zm8+60)
imprimirm(xm1,ym1,zm8+70)

imprimirm(xm16,ym16,zm16)
imprimirm(xm17,ym17,zm17)


errores = c()
errores = c(error(x11, y11))
errores = c(errores,error(x12, y12))
errores = c(errores,error(x13, y13))
errores = c(errores,error(x14, y14))
errores = c(errores,error(x15, y15))
errores = c(errores,error(x16, y16))
errores = c(errores,error(x17, y17))
errores = c(errores,error(x18, y18))
errores = c(errores,error(x19, y19))
errores = c(errores,error(x110, y110))
errores = c(errores,error(x111, y111))
errores = c(errores,error(x112, y112))
errores = c(errores,error(x120, y120))
errores = c(errores,error(x121, y121))


errores = c(errores,error(x1b1, y1b1))
errores = c(errores,error(x1b2, y1b2))
errores = c(errores,error(x1b3, y1b3))
errores = c(errores,error(x1b4, y1b4))
errores = c(errores,error(x1b5, y1b5))
errores = c(errores,error(x1b6, y1b6))
errores = c(errores,error(x1b7, y1b7))
errores = c(errores,error(x1b8, y1b8))
errores = c(errores,error(x1b9, y1b9))

errores = c(errores,error(x1m1, y1m1))
errores = c(errores,error(x1m2, y1m2))
errores = c(errores,error(x1m3, y1m3))
errores = c(errores,error(x1m4, y1m4))
errores = c(errores,error(x1m5, y1m5))
errores = c(errores,error(x1m6, y1m6))
errores = c(errores,error(x1m7, y1m7))
errores = c(errores,error(x1m8, y1m8))

errores = c(errores,error(x1m16, y1m16))
errores = c(errores,error(x1m17, y1m17))

errorProm = 0
mpfr(errorProm, 200)
cat("\n Errores \n")
print(errores)
for(i in 1:length(errores)){
  if(!is.nan(errores[i]))
  {
    errorProm = mpfr(errorProm + errores[i], 200)
  }
  
}

errorProm = mpfr(errorProm/length(errores), 128)

cat("\n Error absoluto")
print(errorProm)
cat("\n Volumen")
print(mpfr(abs(volumen), 128))
