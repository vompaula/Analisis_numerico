n = float(input("Introduce el dato: "))
# E = float(input("Introduce el error permitido: "))
E = pow(10, -9)
x = float(input("Introduce el valor inicial: "))
y = (1/2)*(x+(n/x))
while (abs(x-y) > E) :
  x = y
  y = (1/2)*(x+(n/x))
print ("La raiz cuadrada es: \n", y)