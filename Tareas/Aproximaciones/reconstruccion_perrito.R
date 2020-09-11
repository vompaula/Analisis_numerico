#x1 <- seq(0, 3, length.out=100)
#y1 <- x^2/((x^2)-16)
#x2 <- seq(3, 6, length.out=100)
#y2 <- x^2/((x^2)-16)
#plot(x1, y1, type = "l", col = "blue", lwd = 3)
#lines(x2, y2, col = "blue")
library(ggplot2)

y = c(3,3.7, 3.9, 4.5, 5.7, 6.69, 7.12, 6.7, 4.45, 7,6.1, 5.6, 5.87, 5.15, 4.1, 4.3, 4.1,3)
x = c(1,2,5,6,7.5,8.1,10, 13, 17.6, 20, 23.5, 24.5, 25,26.5, 27.5,28, 29, 30)


y1 = y[1:4]  ; x1 = x[1:4]
y2 = y[4:7]  ; x2 = x[4:7]
y3 = y[7:12]  ; x3 = x[7:12]
y4 = y[12:13] ; x4 = x[12:13]     
y5 = y[13:18] ; x5 = x[13:18]

plot(x, 
     y,
     main = "Perrito",
     xlab = "eje X",
     ylab = "eje Y",
     xlim=c(0,35),
     ylim=c(1,15))

lines(spline(x1, y1, n = 500), col = "blue")
lines(spline(x2, y2, n = 500), col = "blue")
lines(spline(x3, y3, n = 500), col = "blue")
lines(spline(x4, y4, n = 500), col = "blue")
lines(spline(x5, y5, n = 500), col = "blue")