################################################################################
# Demografía - Licenciatura en Estadística                                     #
# Daniel Ciganda / Facundo Morini                                              # 
# Laboratorio: Simulación de eventos discretos en una población cerrada        # 
# 11 de Noviembre de 2025                                                       #   
################################################################################

source("ste.R")

fx <- read.csv(file.path("datos","fx.csv"))
mx <- read.csv(file.path("datos","mx.csv"))

# En la clase anterior construimos una función sim_m para generar simulaciones 
# del evento muerte en una población cerrada. 

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

# En este laboratorio vamos a extender nuestra función para incluir el 
# evento primer hijo.

# Ejercicio:
# Trabajando con otro compañero, generar un modelo sim_pop que incluya también la
# edad al primer nacimiento

ini <- 1900  # año de inicio
fin <- 2000  # año de finalización

sim_pop <- 


################################################################################
# Ejercicio:
# Graficar las curva de supervivencia de la transición al primer hijo y la muerte
# utilizando la población simulada con sim_pop.

N <- 10000
pop <- 

library(survival)

# Muerte
t <- pop[, "edad_mte"]
eventos <- t < Inf 
plot(survfit(Surv(t, eventos)~1), xlab="t", ylab="S(t)", xlim = c(0,110))
t <- ste(N, edades = mx$edad, lambda = mx$h, Haz = T)
H <- t[[2]]
lines(0:101, exp(-H), lwd=3, col=2, lty=2)

# 1er hijo 
t <- pop[pop$sexo == 1, "edad_nac"]
eventos <- t < Inf 
plot(survfit(Surv(t, eventos)~1), xlab="t", ylab="S(t)", xlim = c(0,55))
t <- ste(N, edades = fx$edad, lambda = fx$h, Haz = T)
H <- t[[2]]
lines(0:51, c(rep(1,15),exp(-H)), lwd=3, col=2, lty=2)


# Ejercicio:
# Cual es el problema en este caso? Proponer una solución:


