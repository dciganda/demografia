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
# de una función de riesgo - tasas ecurrencia/exposición por edad-

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

pop$sexo <- sample(1:2, N, prob = c(1-srb, srb), replace = T) 

pop$edad <- 0 #

pop$edad_mte <- Inf #


# crear una variable en pop "t_mte" con la edad a la muerte
pop$t_mte <- ste(N, mx[,1], mx[,2])


# Ejercicio:
# Describir el contenido de las primeras tres filas de la tabla "pop"


# Definimos el año de inicio y de fin de la simulación
ini <- 1900  
fin <- 2000  

# definimos el tiempo actual (reloj) como el año de inico
tiempo <- ini


############################
# SIMULACIÓN               #
############################
# Ahora vamos a comenzar a simular el evento muerte
# Tenemos que escojer el tiempo de espera más corto

# Completar el objeto "rid" (real id) para que tenga la poscición en la tabla 
# que corresponde a la persona que va a experimentar el evento y "t" para que
# tenga el tiempo al evento
rid <- which.min(pop$t_mte)
t <- pop[rid,]$t_mte

# Ahora que sabemos que evento vamos a simular tenemos que
# actualizar el reloj

tiempo <- tiempo + t

# Actualizamos las edades

pop$edad <- pop$edad + t

# Actualizamos los tiempos de espera

pop$t_mte <- pop$t_mte - t

# Ahora podemos simular el evento, es decir actualizar la información en pop
# una vez sucedido el evento

pop[rid,]$t_mte <- Inf
pop[rid,]$edad_mte <- pop[rid,]$edad
pop[rid,]$edad <- NA

# Ejercicio:
# Describir los cambios que se hiceron a "pop"


# Ahora vamos a simular el próximo evento, para eso tenemos que volver a 
# repetir las operaciones anteriores, comenzando por encontrar el tiempo de 
# espera más corto

rid <- which.min(pop$t_mte)
t <- pop[rid,]$t_mte
tiempo <- tiempo + t
pop$edad <- pop$edad + t
pop$t_mte <- pop$t_mte - t
pop[rid,]$t_mte <- Inf
pop[rid,]$edad_mte <- pop[rid,]$edad
pop[rid,]$edad <- NA

# Es claro que esta operación necesita un loop
# Ejercicio:

# Construir un while loop para simular la muerte de todos los individuos en pop.
# Incluir un print the tiempo para monitorear la evolución de la simulación.
# Primero volvemos a crear pop y definir los argumentos iniciales
N <- 10
pop <- data.frame(id = 1:N)  
srb <- 0.515 
pop$sexo <- sample(1:2, N, prob = c(1-srb, srb), replace = T) 
pop$edad <- 0 
pop$edad_mte <- Inf #
pop$t_mte <- ste(N, mx[,1], mx[,2])
# 
ini <- 1900  
fin <- 2000  

tiempo <- ini

while (tiempo < fin){
  
  rid <- which.min(pop$t_mte)
  t <- pop[rid,]$t_mte
  if(t == Inf){break}
  tiempo <- tiempo + t
  pop$edad <- pop$edad + t
  pop$t_mte <- pop$t_mte - t
  pop[rid,]$t_mte <- Inf
  pop[rid,]$edad_mte <- pop[rid,]$edad
  pop[rid,]$edad <- NA
  print(tiempo)

} 

# Ejercicio:
# Escribir una función sim_m que tome como argumentos: 
# N, ini, fin, srb, mx
# que simule los eventos y devuelva el objeto "pop"

sim_m <- function(N, ini, fin, mx, srb){
  
  pop <- data.frame(id = 1:N)  
  pop$sexo <- sample(1:2, N, prob = c(1-srb, srb), replace = T) 
  pop$edad <- 0 
  pop$edad_mte <- Inf #
  pop$t_mte <- ste(N, mx[,1], mx[,2])
  
  tiempo <- ini
  
  cat("inicio ok, comenzando loop\n")
  
  while (tiempo < fin){
    
    rid <- which.min(pop$t_mte)
    t <- pop[rid,]$t_mte
    if(t == Inf){break}
    tiempo <- tiempo + t
    pop$edad <- pop$edad + t
    pop$t_mte <- pop$t_mte - t
    pop[rid,]$t_mte <- Inf
    pop[rid,]$edad_mte <- pop[rid,]$edad
    pop[rid,]$edad <- NA
    print(tiempo)
    
  } 
  
  return(pop)
}

pop_m <- sim_m(N = 1000, ini = 1900, fin = 2000, mx, srb = 0.515) 


# Ejercicio:
# Trabajando con otro compañero, generar una simulación que incluya también la
# edad al primer nacimiento

# función para simular eventos en una población cerrada

ini <- 1900  # año de inicio
fin <- 2000  # año de finalización


sim_pop <- function(N, ini, fin, fx, mx, srb){
  
  ###########################################################
  # Población Inicial                                       #  
  ###########################################################
  pop <- data.frame(id = 1:N)
  pop$sexo <- sample(1:2, N, prob = c(1 - srb, srb), replace = TRUE) # mujer = 1
  pop$edad <- 0
  pop$edad_nac <- Inf
  pop$edad_mte <- Inf
  pop$te_nac <- Inf
  
  idm <- which(pop$sexo == 1)
  
  pop$te_nac[idm] <- ste(length(idm), fx[,1], fx[,2])
  pop$t_mte <- ste(N, mx[,1], mx[,2])
  
  # Rearranging columns
  pop <- pop[, c("te_nac", "t_mte", "id", "sexo", "edad", "edad_nac", "edad_mte")]
  
  nom_e <- c("nac", "mte")
  
  cat("inicio ok, comenzando loop\n")  ######################################## 
  
  #************************************************************************************************
  #---------------- Corremos la simulación --------------------------------------------------------
  #************************************************************************************************
  tiempo <- ini
  
  while (tiempo < fin){
    
    # Find the next event
    event_times <- as.matrix(pop[, c("te_nac", "t_mte")])
    min_idx_flat <- which.min(event_times)
    min_idx <- arrayInd(min_idx_flat, dim(event_times))
    t <- as.numeric(event_times[min_idx[1], min_idx[2]])  # Ensure t is numeric scalar
    prox_e <- nom_e[min_idx[2]]
    rid <- min_idx[1]
    
    #************************************************************************************************
    #--------- Actualizando--------------------------------------------------------------------------
    #************************************************************************************************
    # Reloj
    print(tiempo)
    tiempo <- tiempo + t
    
    # Edades 
    pop$edad <- pop$edad + t
    
    # Duraciones
    pop$te_nac <- pop$te_nac - t
    pop$t_mte <- pop$t_mte - t
    
    ########################
    ##     NACIMIENTO     ##
    ########################
    if (prox_e == "nac"){
      
      print("nacimiento")
      
      # Información madre
      pop[rid, ]$te_nac <- Inf
      pop[rid, ]$edad_nac <- pop[rid, ]$edad
      
      # Nuevo individuo  
      new_id <- max(pop$id, na.rm = TRUE) + 1
      new_ind <- data.frame(
        te_nac = Inf,
        t_mte = ste(1, mx[,1], mx[,2]),
        id = new_id,
        sexo = ifelse(runif(1) > srb, 1, 2),
        edad = 0,
        edad_nac = Inf,
        edad_mte = Inf
      )
      
      if (new_ind$sexo == 1) {
        new_ind$te_nac <- ste(1, fx[,1], fx[,2])
      }
      
      pop <- rbind(pop, new_ind)
    }
    
    ########################
    ##      MUERTE        ##
    ########################
    if (prox_e == "mte") {
      
      print("muerte")
      
      pop[rid, ]$t_mte <- Inf
      pop[rid, ]$te_nac <- Inf
      pop[rid, ]$edad_mte <- pop[rid, ]$edad
    }
    
  }
  
  return(pop)
  
}

pop <- sim_pop(N = 1000, ini = 1900, fin = 2000, fx, mx, srb = 0.515)



################################################################################
# Ejercicio:

# 1) Graficar las curva de supervivencia de la transición al primer hijo y la muerte
#    utilizando exclusivamente información de individuos "nacidos" en la simulación 

# generando datos
N = 10000
pop <- sim_pop(N, ini = 1900, fin = 2100, fx, mx, srb = 0.515)

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


# 1er hijo - con datos censurados
t <- pop[pop$sexo == 1, "edad_nac"]
eventos <- t < Inf 
nt <- pop[pop$sexo == 1 & pop$edad_nac == Inf, "edad_mte"]
pop[pop$sexo ==1 & pop$edad_nac == Inf, "edad_nac"] <- nt
t <- pop[pop$sexo ==1, "edad_nac"]
plot(survfit(Surv(t, eventos)~1), xlab="t", ylab="S(t)", xlim = c(0,55))
t <- ste(N, edades = fx$edad, lambda = fx$h, Haz = T)
H <- t[[2]]
lines(0:50, c(rep(1,15),exp(-H)), lwd=3, col=2, lty=2)





