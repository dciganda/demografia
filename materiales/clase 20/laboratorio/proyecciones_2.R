################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              # 
# Laboratorio: Proyecciones de Población                                       # 
# 20 de Noviembre de 2025                                                      #   
################################################################################
#######################
#  MATRIZ DE LESLIE   #
#######################

# El procedimiento que utilizamos anteriormente para proyectar una población 
# estructurada por edad, puede pensarse como la multiplicación de una matriz
# (la matriz de Leslie) por un vector con la población base. La subdiagonal 
# inferior contiene las probabilidades de superviviencia y la primera fila
# contiene las tasas de fecundidad para cada intervalo de edad

# El objetivo de esta sección es construir la matriz y utilizarla para obtener 
# la proyección de una población base.

# Cargamos los datos
sw <- read.table("datos/sweden_data_1993.txt", header=F)

# Agregamos nombres a las columnas: "age", "p93", "L", "f"
names(sw) <- c("age", "p93", "L", "f")

# definimos el sex ratio at birth
srb <- 1.05

# Salvamos las columnas relevantes en objetos para facilitar la lectura del código
L <- sw$L
f <- sw$f
N <- sw$p93

# los valores de la tabla deben estar en base 1. Por eso divididmos las Ls sobre l0
L <- L/100000

# definir una matriz de largo = ancho = length(L)
n = length(L)
M <- #completar

# asignar las probabilidad de supervivencia en los intervalos en la subdiagonal inferior
px <- #completar
# completar <- px

# asignar las probabilidad de supervivencia para los grupos de edad en el intervalo abierto en t+n
M[n,n-1] <- M[n,n] <- #completar
  
# asignar la tasas de maternidad en la primera fila

# Ejercicio:
# Crear una función "leslie" para construir la matriz que tome como parámetros f y L
# Proyectar la población femenina de Suecia en el 93 al 98, utilizando la matriz
  
for(i in 1:(n-1)) {
  # completar
}

# Ejercicio:
# Crear una función "leslie" para construir la matriz que tome como parámetros f y L
# Proyectar la población femenina de Suecia en el 93 al 98, utilizando la matriz

leslie <- function(L, m) {
  
  # completar
  
}  

Ml <- leslie(L, f)

sw$p98 <- Ml %*% sw$p93


# Ejercicio:
# Crear una función "p_pop" que tome las tasas específicas de fecundidad "f", los años persona "L"
# y un vector incial de población "N0" y proyecte la población hacia el futuro.
# agregar un parametro "iter" con el número de iteraciones / intervalos de proyección.

p_pop <- function(N0, f, L, iter){
  
  # completar
  
}
  
