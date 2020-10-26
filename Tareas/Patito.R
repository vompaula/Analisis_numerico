#Ejercicio: Elaborar la silueta de un pato
#Elaborado por: Laura Mariana Jimenez, Paula Valentina Sanchez, Sebasti?n Guti?rrez
#An??isis Num?rico 20-30


x = c( #Parte de arriba
       0.9, 1.9,  2.6, 3.0,  4.7,   6.0, 8.0,  10.5,  11.6, 12.6, 13.0, 13.3,
       #Parte inferior derecha
       #12.763, 11.501, 10.572, 9.337, 8.4703,
       12.763, 12.364, 11.501, 10.572,  9.337, 8.473,
       #Ala parte derecha
       8.141, 6.606, 5.839, 5.289, 4.990, 4.750, 4.659,
       #Ala parte izquierda
       4.82, 5.3,  
       #Parte inferior izquierda
       5.5 , 5, 4.44, 3.5, 2.34 , 2, 1, 1.12, 1.2, 0.9)


y = c(#Parte de arriba
      1.3, 1.85, 2.6, 2.7, 2.05, 2.25, 2.25, 1.4,  0.7,  0.5,  0.4,  0.25, 
      #parte inferior derecha
      #0.120,-0.085, -0.005, -0.494, -0.434,
      0.120, 0.093, -0.085, -0.005, -0.494, -0.434,
      #Ala parte derecha
      -1.138, -4.286, -4.982, -5.268, -5.284, -5.259, -5.161,
      #Ala parte izquierda
      -5, -3,
      #Parte inferior izquierda
      0, 0.53, 0.9, 1.11, 1.23, 1.18, 1.12, 1, 0.98, 1.3)
  

plot(x,y,main="Patito", pch=20, cex=1,asp=1)

Interpolar<-function(i, f){
  x1 = x[i:f]
  y1 = y[i:f]
  lines(spline(x1, y1, n = 201), col = "green")
}


#Parte de arriba
Interpolar (1, 2)
Interpolar (2, 5)
Interpolar (5, 9)
Interpolar (9, 12)

#parte inferior derecha
Interpolar (12, 14)
Interpolar (14, 16)
Interpolar (16, 18)

#Ala parte derecha
Interpolar (18, 19)
Interpolar (19, 25)

#Ala parte izquierda
Interpolar (25, 27)
Interpolar (27, 28)

#Parte inferior izquierda
Interpolar(28, 30)
Interpolar(30, 39)
