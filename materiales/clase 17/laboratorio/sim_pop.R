################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Simulación de eventos discrteos en una población cerrada        # 
# 14 de Noviembre de 2024                                                      #   
################################################################################

# El objetivo de este laboratorio es construir una simulación de eventos discretos 
# para representar trayectorias demográficas individuales. Esas trayectorias incluirán
# los eventos primer hijo y fallecimiento.


# cargamos nuestra función para simular tiempos al evento a partir
# de una función de riesgo - tasas condicionales por edad-

source("ste.R")

# cargamos los datos
fx <- read.csv(file.path("datos","fx.csv"))
mx <- read.csv(file.path("datos","mx.csv"))


############################
# POBLACIÓN INICIAL        #
############################
# Comenzamos por definir una población inicial con N individuos


# Ejercicio: describir el contenido de cada linea con #

N <- 10
pop <- data.frame(id = 1:N) # 

srb <- 0.515 # 

# Asignar un sexo a cado uno de los integrantes de "pop" (1 = mujer, 2 = hombre)
# usando sample con argumento prob = c(1-srb, srb)

pop$sexo <-

pop$edad <- 0 #

pop$edad_mte <- Inf #


# crear una variable en pop "t_mte" con la edad a la muerte
pop$t_mte <- 


# Ejercicio:
# Describir el contenido de las primeras tres filas de la tabla "pop"


  
############################
# SIMULACIÓN               #
############################
# Definimos el año de inicio y de fin de la simulación
ini <- 1900  
fin <- 2000  

# definimos el tiempo actual (reloj) como el año de inico
tiempo <- ini


# Ahora vamos a comenzar a simular el evento muerte
# Tenemos que escojer el tiempo de espera más corto

# Completar rl objeto "rid" (real id) para que tenga la poscición en la tabla 
# que corresponde a la persona que va a experimentar el evento y "t" para que
# tenga el tiempo al evento
rid <- 
t <- 
  
# Ahora que sabemos que evento vamos a simular tenemos que
# actualizar el reloj

tiempo <- 

# Actualizamos las edades

pop$edad <- 

# Actualizamos lo tiempos de espera

pop$t_mte <- 

# Ahora podemos simular el evento, es decir actualizar la información en pop
# una vez sucedido el evento

pop[rid,]$t_mte <- Inf
pop[rid,]$edad_mte <- 
pop[rid,]$edad <- NA

# Ejercicio:
# Describir los cambios que se hiceron a "pop"

# Ahora vamos a simular el próximo evento, para eso tenemos que volver a 
# repetir las operaciones anteriores, comenzando por encontrar el tiempo de 
# espera más corto

rid <- 
t <-
tiempo <-
pop$edad <- 
pop$t_mte <- 
pop[rid,]$t_mte <-
pop[rid,]$edad_mte <- 
pop[rid,]$edad <- 

# Es claro que esta operación necesita un loop
# Ejercicio:

# Construir un while loop para simular la muerte de todos los individuos en pop.
# Incluir un print de tiempo para monitorear la evolución de la simulación.
# Primero volvemos a crear pop y definir los argumentos iniciales
N <- 10
pop <- data.frame(id = 1:N)  
srb <- 0.515 
pop$sexo <- 
pop$edad <- 0 
pop$edad_mte <- Inf #
pop$t_mte <- 
# 
ini <- 1900  
fin <- 2000  

tiempo <- ini

while (tiempo < fin){
  

} 

# Ejercicio:
# Escribir una función sim_m que tome como argumentos: 
# N, ini, fin, srb, mx
# que simule los eventos y devuelva el objeto "pop"

sim_m <- function(N, ini, fin, mx, srb){
  
  
}


# Ejercicio:
# Trabajando con otro compañero, generar un modelo sim_pop que incluya también la
# edad al primer nacimiento

# función para simular eventos en una población cerrada

ini <- 1900  # año de inicio
fin <- 2000  # año de finalización


sim_pop <- 


################################################################################
# Ejercicio:

# 1) Graficar las curva de supervivencia de la transición al primer hijo y la muerte
#    utilizando exclusivamente información de individuos "nacidos" en la simulación 

# generando datos
N = 10000
pop <- 

library(survival)
# muerte
t <- pop[, "edad_mte"]
eventos <- t < Inf 
plot(survfit(Surv(t, eventos)~1), xlab="t", ylab="S(t)", xlim = c(0,110))
t <- ste(N, edades = mx$edad, lambda = mx$h, Haz = T)
H <- t[[2]]
lines(0:100, exp(-H), lwd=3, col=2, lty=2)

  
# 1er hijo 
t <- pop[pop$sexo == 1, "edad_nac"]
eventos <- t < Inf 
plot(survfit(Surv(t, eventos)~1), xlab="t", ylab="S(t)", xlim = c(0,55))
t <- ste(N, edades = fx$edad, lambda = fx$h, Haz = T)
H <- t[[2]]
lines(0:50, c(rep(1,15),exp(-H)), lwd=3, col=2, lty=2)


# Ejercicio:
# Cual es el problema en este caso? Proponer una solución:


