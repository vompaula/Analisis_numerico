n<-9 
E<-10^-9
x<-1
y<-((1/2)*(x+(n/x)))
cont<-0
while(abs(x-y)>E)
{
  x<-y
  y<-(1/2)*(x+(n/x))
  cont<- cont + 1
}
cat("La raiz es ", y, "\n")
cat ("El numero de iteraciones fue ", cont)


