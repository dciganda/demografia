################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Proyecciones de Población                                       # 
# 21 de Noviembre de 2024                                                      #   
################################################################################

# El objetivo es proyectar una población estructurada por edad, en este caso la
# población de Suecia, utilizando información sobre las probabilidades de 
# supreviviencia en el intervalo de la proyección y unas tasas específicas de 
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
px[n-1] <- px[n] <- 

  
# Calculamos un vector auxiliar con la población en 1998
  
p98_aux <- N * px  

# calcular la población mayor de 5 en 1998 (usand la información en p98_aux)
sw$p98 <- NA
sw$p98[2:(n-1)] <- 
sw$p98[n] <- 
  
  
# definimos un vector vacio "B" para guardar en los próximos pasos los nacimientos
B <- vector()  
  
# Calcular los nacimientos entre t y t+n 
idf <- which(f>0)

for(i in idf){
  B[i-min(idf-1)] <- #completar
}
  
  
# calcular los nacimientos de mujeres 
Bx_f <- 
  
# calcular la población de edad 0-5 en 1998
sw$p98[1] <- 

  

# Ejercicio: repetir el procedimiento para proyectar la población a 2003
# Reproducir la tabla presentada en clase con las poblaciones a cada año y los nacimientos 
# por edad

