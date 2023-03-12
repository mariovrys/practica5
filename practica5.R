#1
#Para crear un vector se usa la función c() y se le asocia un nombre. 
numArtefactos <- c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
typeof(numArtefactos) #Para averiguar la forma de representar los numeros (integer o double)
numArtefactos_int <- as.integer(numArtefactos) #Para convertir un objeto a un numero entero. 
is.integer(numArtefactos) #Para averiguar si un numero es entero (TRUE) o no (FALSE). 

#2
#La media es el resultado obtenido tras la suma de todos los valores dividido entre el numero de
  #valores y se calcula de la siguiente manera. 
media <- mean(numArtefactos_int)
media #Para ver en la consola el resultado de la función, dando un resultado de 45,55

#3
# La mediana es el valor que se ubica en el centro de todos una vez ordenados de
  #mayor a menor estos valores y se calcula 
mediana <- median(numArtefactos_int)
mediana #La mediana es de 40,5

#4
#La moda es el valor que más se repite en una secuencia y se puede calcular de la siguiente manera
numArtefactos_int
moda <- function(x) {
  u <- unique(numArtefactos_int)
tab <- tabulate(match(x,u))
u[tab == max(tab)] 
}
moda(numArtefactos_int) #La moda es de 10

#5
table(numArtefactos_int)

#6
#Los cuartiles clasifican la informacion en base al grupo al que pertenecen. 
quantile(numArtefactos_int, c(0.25, 0.5, 0.75, 1))

#7
#El rango intercuartilico es la diferencia entre el tercer y primer cuartil y se calcula
#con la funcion IQR. 
rinterc <- IQR(numArtefactos_int)
rinterc #El rango intercuartilico es de 40

#8
#El rango es un valor que muestra la diferencia entre el valor maximo y el valor minimo y se calcula
#con la funcion range. 
rango <- range(numArtefactos_int, na.rm = FALSE)
rango

#9
#La varianza es la medida de dispersion que muestra la variabilidad de los datos respecto a la media aritmetica y se expresa con la funcion var.  
var <- var(numArtefactos_int)
var #La varianza es de 927,1026

#10
#La desviacion estandar es una medida usada para el calculo de la variacion de los datos con respecto a la media. 
desest <- sd(numArtefactos_int) #Con la funcion sd y nombrando al objeto numArtefactos_int
    # se crea la desviación estandar. 
desest #La desviacion estandar es de 30,44836

#11
#La diferencia entre desviacion estandar y varianza radica en que la primera es la raiz cuadrada de la segunda, siendo valores distintos. 

#12
boxplot(numArtefactos_int, horizontal = TRUE)
#Para visualizar de manera horizontal la dispersion de numArtefactos_int se puede usar un diagrama de caja y bigotes con la funcion
#boxplot. 

#13
vector3 <- c(21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1) #Para crear un vector de nuevo se utiliza la funcion
#c() con los valores que queremos dentro del paentesis. 

#14
#Forma 1
#C. variacion  numArtfactos_int
cvnar <- sd(numArtefactos_int) / mean(numArtefactos_int) * 100
cvnar

#C. variacion vector3
cvv3 <- sd(vector3) / mean(vector3) * 100
cvv3

#Para hacer el coeficiente de variacion de numArtefactos_int y de vector3 se pueden utilizar las funciones que ya
#hemos usando anteriormente de sd y mean con los diferentes objetos entre parentesis. En el caso del c. de variacion de
#numArtefactos_int corresponde a 66,84602 y para vector3 de 63,59607. Esto permite conocer las desviaciones que hay
#entre los datos con respecto a la media y de los propios datos entre sí. 

#Forma 2
coef_var(numArtefactos_int)
coef_var(vector3)

#15
#1
stats_summary <- function(numArtefactos_int) {
  mean <- mean(numArtefactos_int)
  median <- median(numArtefactos_int)
  sd <- sd(numArtefactos_int)
  iqr <- IQR(numArtefactos_int)
  range <- max(numArtefactos_int) - min(numArtefactos_int)
  cv <- sd / mean * 100
  
  stats_table <- data.frame(valores = c("Media", "Mediana", "Desviacion_estandar", "Rango_intercuartilico", "Rango", "Coeficiente_Variacion"),
                            valor = c(mean, median, sd, iqr, range, cv))
  return(stats_table)
}

stats_summary(numArtefactos_int) #Para ver la tabla en la consola. 

#2
stats_summary <- function(vector3) {
  meanr3 <- mean(vector3)
  medianr3 <- median(vector3)
  sdr3 <- sd(vector3)
  iqrr3 <- IQR(vector3)
  ranger3 <- max(vector3) - min(vector3)
  cvr3 <- sdr3 / meanr3 * 100
  
  stats_table <- data.frame(valores = c("Media", "Mediana", "Desviacion_estandar", "Rango_intercuartilico", "Rango", "Coeficiente_Variacion"),
                            valor = c(meanr3, medianr3, sdr3, iqrr3, ranger3, cvr3))
  return(stats_table)
}
stats_summary(vector3) #Para ver la tabla con los valores en la consola. 

#16
install.packages("e1071") 
library(e1071) #Para utilizar la funcion correspondiente para el coeficiente de asimetria es encesario instalar la libreria e1071 y cargarla. 

cas <- skewness(vector3) #Se asocia la funcion a "cas" y se calcula el coeficiente de asimetria de vector3 con la funcion skewness. 
cas
#El coeficiente de asimetria de vector3 es de 0,3138528. Al ser mayor que 0, la distribucion no es simetrica indicando que tenemos valores
#mayores que la media, una asimetria positiva. En caso de obtener un coeficiente de asimetria menor que 0, la distribucion es a la izquierda, una distribucion
#negativa. 

#17
#La curtosis es un valor que muestra la proximidad de los datos conrespecto a su valor medio. 
install.packages("moments") #Se necesita intalar el paquete moments
library(moments)

curt <- kurtosis(vector3) #Se utiliza la funcion kurtosis del paquete moments
curt
#La curtosis es de 1,952376, una curtosis leptocurtica (una concentracion de valores que rondan la media). 