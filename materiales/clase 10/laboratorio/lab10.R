################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              # 
# 10mo Laboratorio: Tablas de Mortalidad                                       # 
# 18 de Septiembre de 2025                                                     #   
################################################################################

###########################################################
# TABLA DE MORTALIDAD DE PERíODO                          #
###########################################################

# El objetivo es construir una tabla de mortalidad utilizando la información 
# más comunmente disponible en países que cuentan con sistemas de estadísticas
# vitales y censos de población.

########################
# EXPLORAR LOS DATOS   #
########################

# Vamos a utilizar los datos de las tasas específicas de mortalidad por edad,
# Mx, para Dinamarca
# Estos datos se encuentran disponibles en la Human Mortality Database 
# www.mortality.org

# Cargamos los datos
Mx_all <- read.csv(file.path("datos","Mx_1x1_Denmark.txt"),
                   sep = "", skip = 1, header = T)

# Graficamos las Mx utilizando la función plot_Mx para los anios
# 1900:2005 en escala logarítmica para hombres y mujeres 
# (sex = "Male" / "Female") de edades 0 a 95.

source("plot_fun.R")
plot_Mx(dat = Mx_all, anios = 1900:2005,edades = 0:95, sex = "Male")
plot_Mx(dat = Mx_all, anios = 1900:2005,edades = 0:95, sex = "Female")

# Describir lo que se observa en el gráfico. Contestando: 
# Qué indicador estamos utilizando? Qué hay en numerador/denominador?
# Cuáles son las diferencias más imprtantes entre hombres y mujeres?
# Cuál es la tendencia en el tiempo?



#######################################################
# CONSTRUCCION DE LA TABLA DE MORTALIDAD DE PERÏODO   #
#######################################################

# Extraer las tasas de mortalidad por edad Mx para hombres de 0 a 95 años 
# en Dinamarca en 1900 utilizando la función plot_Mx con argumento
# return_data = T, también especificar log_escale = F

nMx <- 


# Describir que hace la siguiente función:
get_na0 <- function(nMx, males){
  
  if(males){
    
    if (nMx[1] < 0.023){
      
      na0 <- 0.14929 - 1.99545 * nMx[1]
    }else{
      
      if(nMx[1] >= 0.023 & nMx[1] < .08307){
        
        na0 <- .02832 + 3.26021 * nMx[1]
        
      }else{
        
        na0 <- 0.29915
      }
    }
  }else{
    
    if (nMx[1] <  0.01724){
      
      na0 <- .14903 - 2.05527 * nMx[1]
    }else{
      
      if(nMx[1] >= 0.01724 & nMx[1] < 0.06891){
        
        na0 <- 0.04667 + 3.88089 * nMx[1]
        
      }else{
        
        na0 <- .31411
      }
    }  
    
  }
  
  return(na0)
  
}

# Definir las edades
x <- 

# Definimos el número de intervalos
nmax <- 

# Definimos los factores de separación nax 
# creamos un vector vacio para guardar los nax
nax <- vector()

# definimos a0 con la ayuda de la función "get_na0"
na0 <- 

# assignamos los factores a cada intervalo
nax[1] <- 
nax[2:nmax] <- 

# convertimos las nMx en nqx
nqx <- 

# nos aseguramos que la probabilidad en el último intervalo sea 1
nqx[nmax] <- 

# Construimos las lx - ver cumprod() - Nota: l0 = 1
lx <- 

# Obtenemos las defunciones
ndx <- 

# creamos un vector con los sobrevivientes en x+n
lxn <- 

# Obtenemos los años persona en el intervalo nLx
nLx <- 

# Calculamos los años persona por encima de x
Tx <- 

# Calculamos la esperanza de vida a edad x
ex <- 

# Creamos la tabla
lt <- data.frame(x, nax = round(nax, 4),
                 nMx = round(nMx,4),
                 nqx = round(nqx[1:nmax], 4), lx = round(lx[1:nmax],4),
                 ndx = round(ndx, 4), nLx = round(nLx, 4), Tx = round(Tx, 
                                                                      2), ex = round(ex, 2))
lt

# Ejercicio:

# crear una función llamada "compute_lt" que va a tomar los siguientes argumentos:
# 1 - nMx: Un vector de tásas específicas de mortalidad por edad observadas. 
# 2 - x: Un vector con las edades de inicio de cada intervalo
# 3 - sex: Define si el análisis está hecho para hombres ("M") o mujeres ("F")
# 4 - tabla: si TRUE la función devuelve toda la tabla, si FALSE devuelve sólo el valor de la esperanza de 
# vida al nacer (hacer TRUE por default)
