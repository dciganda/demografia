################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Proyecciones de Población                                       # 
# 26 de Noviembre de 2024                                                      #   
################################################################################

# El objetivo es proyectar una población estructurada por edad, en este caso la
# población de Suecia, utilizando información sobre las probabilidades de 
# supreviviencia en el intervalo de la proyección y unas tasa específicas de 
# fecundidad por edad.

# Vamos a trabajar con una población cerrrada y compuesta sólo por mujeres.

# Cargamos los datos
sw <- 

# Agregamos nombres a las columnas: "age", "p93", "L", "f"


# Ejercicio: Describir el contenido de cada columna de la tabla

# definimos el sex ratio at birth
srb <- 1.05

# Salvamos las columnas relevantes en objetos para facilitar la lectura del código
L <- sw$L
f <- sw$f
N <- sw$p93


# guardamos el largo de L en un objeto "n"


# calcular las probabilidades de supervivencia en cada intervalo en un vector "px"
px <- 
  
# agregar la probabilidad en el intervalos abierto (últimos dos intervalos)
px[n-1] <- 


# calcular la población mayor de 5 en 1998
sw$p98 <- NA
sw$p98[2:(n-1)] <- 
sw$p98[n] <- 
  
  
# definimos un vector vacio "B" para guardar en los próximos pasos los nacimientos
  
  
# Calcular los nacimientos entre t y t+n y guardarlos en un vector "Bx"
  
  
# calcular los nacimientos de mujeres 
Bx_f <- 
  
# calcular la población de edad 0-5 en 1998
sw$p98[1] <- 

  

# Ejercicio: repetir el procedimiento para proyectar la población a 2003
# Reproducir la tabla presentada en clase con las poblaciones a cada año y los nacimientos 
# por edad

#######################
#  MATRIZ DE LESLIE   #
#######################

# El procedimiento anterior puede pensarse como la multiplicación de una matriz (la matriz de Leslie)
# por un vector con la población base. La subdiagonal inferior contiene las probabilidades de superviviencia
# y la primera fila contiene las tasas de fecundidad para cada intervalo de edad

# El objetivo de esta sección es construir la matriz y utilizarla para obtener la proyección de una población base.

# los valores de la tabla deben estar en base 1. Por eso divididmos las Ls sobre l0
L <- L/100000

# definir una matriz de largo = ancho = length(L)

# asignar las probabilidad de supervivencia en los intervalos en la subdiagonal inferior

# asignar las probabilidad de supervivencia para los grupos de edad en el intervalo abierto en t+n

# asignar la tasas de fecundidad en la primera fila

# Ejercicio:
# Crear una función "leslie" para construir la matriz que tome como parámetros f y L
# Proyectar la población femenina de Suecia en el 93 al 98, utilizando la matriz




# Ejercicio:
# Crear una función "p_pop" que tome las tasas específicas de fecundidad "f", los años persona "L"
# y un vector incial de población "N0" y proyecte la población hacia el futuro.
# agregar un parametro "iter" con el número de iteraciones / intervalos de proyección.


  
