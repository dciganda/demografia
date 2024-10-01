################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 11vo Laboratorio: Tablas de Mortalidad                                       # 
# 1 de Octubre de 2024                                                         #   
################################################################################

###########################################################
# TABLA DE MORTALIDAD DE PERíODO                          #
###########################################################

source("plot_fun.R")

# Cargamos los datos
Mx_all <- read.csv(file.path("datos","Mx_1x1_Denmark.txt"),
                   sep = "", skip = 1, header = T)

# Extraer las tasas de mortalidad por edad Mx para hombres de 0 a 95 años en 
# Dinamarca en 1900 utilizando la función plot_Mx con argumento return_data = T,
# también especificar log_escale = F

nMx <- plot_Mx(dat = Mx_all, anios = 1900,edades = 0:95,
               sex = "Male", return_data = T, log_escale = F)


# traemos la función "compute_lt" trabajada en el práctico anterior y corregimos
# los años-persona en el intervalo abierto 

compute_lt <- 


#####################################################
# Validación                                        #
#####################################################
# El paquete`demogR` contiene una función para calcular una tabla de vida
# vamos a usar esta función para validar los resultados obtenidos 
# con los cálculos anteriores

# Cargamos el paquete
library(demogR)

# La función recibe como inputs un vector de defunciones por edad y un vector 
# con la exposición al riesgo en cada tramo de edad (años-persona).
# A partir de estos inputs calcula las tasas de mortalidad,
# convierte esas tasa en probabilidades de morir y calcula el resto de la tabla.

# Cargamos los datos - obtenidos de Human Mortality Database -

ndx_all <- # defunciones
nkx_all <- # exposición
  
# Obtenemos las defunciones / exposición al riesgo para hombres, edad 0:95 en 1900
ndx <- 
nkx <- 

# Calculamos la tabla - `type` refiere a los factores de separación,
# la opción "cd" es la más similar al procedimiento que utilizamos nosotros
ltd <- life.table()

# Comparamos resultados
plot(compute_lt(nMx, 0:95, sex = "M", tabla = T)$ex)
points(ltd$ex,col = "red")

#####################################################
# Análisis de la Evolución de la Esperanza de Vida  #
#####################################################

# Obtener las tasas de mortalidad para los hombes de edades 0 a 95
# desde 1900 a 2021. El argumento "as_list" de la función "plot_Mx" tiene
# que estar en "T" para obtener una lista con las tasas para cada año
# en cada elemento de la lista

nMxc_M <- 

# Obtener los mismos resultados para Mujeres
nMxc_F <- 

# Obtener la esperanza de vida al nacer para cada año 
  
exc_M <- 
exc_F <- 

plot(1900:2021, exc_F)
points(1900:2021, exc_M, col = "red")

# Describir los resultados 



