#######################################################################################
# Demografía 2023 IESTA                                                               #
# Daniel Ciganda                                                                      # 
# 9no Laboratorio: Tablas de Mortalidad                                               # 
# 15 de Septiembre de 2023                                                            #   
#######################################################################################

# El objetivo es construir una tabla de mortalidad para una cohorte 

# Disponemos de datos con la edad exacta a la muerte dx (en años) para 
# 10 personas nacidas el 1° de enero de 1800
dx <- c(71.55, 1.22, 62.91,59.60,0.07,
       22.12,71.14,16.41, 64.05, 76.79)

sort(dx)

# numero de observaciones
N <- 

# vamos a realizar los cálculos para los intervalos
# que comienzan en las siguientes edades exactas x:
x <- 

# tamaño de los intervalos ver diff()
n <- 

######################################  
# defunciones entre edad x, x+n      #
######################################
# Empezamos por generar la primera columna de nuestra tabla: ndx

# Intervalo en el que se registra cada defunción ver findinterval()
di <- 

# defunciones
ndx <- 
  
# Creamos la tabla con las primeras dos columnas:
# edad exacta y defunciones
lt <- cbind(x, ndx)
lt

#############################
# sobrevivientes a edad x   #
#############################
# Para esto nos puede ayudar calcular la suma acumulada de defunciones 
# a edad exacta x
lx <- 

# añadimos la columna lx a la tabla
lt <- cbind(lt, lx)
lt

#######################################
# probabilidad de morir entre x, x+n  #
#######################################
nqx <- 

##########################################################  
# probabilidad de sobrevivir entre la edad x a edad x+n  #
##########################################################
npx <- 

# añadimos las columnas qx, px
lt <- cbind(lt, nqx, npx)
lt

#####################################################
# años persona vividos entre edad x, x+n            #
#####################################################
# emepzamos por obtener los años persona vividos por cada 
# individuo en el intervalo 
ap <- 

# ahora sumamos los años persona en cada intervalo - ver by()   
sum_ap <- as.vector(by(ap, di, sum))

# ahora agregamos los intervalos sin fallecimientos
all_ap <- 
all_ap[unique(di)] <- 

Lx <- 

# añadimos la columna Lx
lt <- cbind(lt, Lx)
lt

####################################################
# anios persona vividos por encima de la edad x    #
####################################################
# ver cumsum() y rev()
Tx <- 

# añadimos la columna Tx
lt <- cbind(lt, Tx)
lt

#################################
# Esperanza de vida a edad x    #
#################################
ex <- 

# añadimos la columna ex
lt <- cbind(lt, ex)
lt

# Ejercicio: Describa el contenido de cada columna en lt 


