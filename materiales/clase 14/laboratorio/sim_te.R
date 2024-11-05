################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Simulación de tiempos a distintos eventos demográficos          # 
# 04 de Noviembre de 2024                                                      #   
################################################################################

# El objetivo de este módulo es simular tiempos de espera o duraciones a los
# eventos fallecimiento y primer hijo. Para esto partimos de unas tasas
# específicas de mortalidad y unas tasas específicas de transición
# a la maternidad. 
# Se asume que el riesgo de los eventos es *constante* en cada intervalo (edades)
# Bajo este supuesto, las tasas de ocurrencia/exposición representan 
# el riesgo del evento. Es decir que el vector de tasas específicas
# representa la función de riesgo del evento.


###############
#   UNIROOT   #
###############
# Para simular nuestros tiempos de espera va a ser necesario utilizar 
# la función uniroot
# Esta función nos va perimitir encontrar la raiz de una función, ej.:
f <- function(x){x^2 - 4}


curve(f, from = -5, to = 5); abline(h = 0, lty = 3)
# cuáles son las raices de f()?


# Uniroot recive tres argumentos, la función y los límites del intervalo donde 
# va a buscar la función 
uniroot(f, lower = -5, upper = 0)$root # Nos da la raiz en [-5 , 0]
uniroot(f, lower = 0, upper = 5)$root # Nos da la raiz en [0 , 5]

# Cuando el intervalo incluye más de una raiz la función no devuelve resultados
uniroot(f, lower = -5, upper = 5)$root


###############
#    DATA     #
###############
# Cargamos las tasas condicionales de fecundidad por edad al 1er nacimiento 
# para una cohorte.
fert <- read.csv(file.path("datos","fx.csv"))
plot(fert)

# Cargamos las tasas de mortalidad por edad para una cohorte
mort <- read.csv(file.path("datos","mx.csv"))
plot(mort)

# Estas tasas van a representar nuestra función de riesgo constante a intervalos para 
# las variables aleatorias "Tiempo al Fallecimiento" y "Tiempo al Primer Hijo" 

###########################################
#    SIMULACION DE TIEMPOS DE ESPERA      #
###########################################
# Comenzamos por definir los inputs para el procedimiento.
# Definimos el limite superior e inferior de los intervalos
# esto es importante si queremos llegar a una función que 
# acepte intervalos de diferentes largos

# intervalos
edades <- 
inf <- 
sup <- 
lambda <- 

# función de riesgo por intervalos
# Describir que hacen cada uno de los pasos
h.pw <- function(t, inf, sup, lambda){
  
  lower_int <- (t-inf)>=0 #
  upper_int <- (t-sup)<0 #
  indicator <- lower_int * upper_int #
  
  max(lambda * indicator) # 
  
}

# Ejemplo
h.pw(t=1, inf, sup, lambda)

# Que devuelve la función en este caso?


# función de riesgo acumulado
# Describir cada uno de los pasos
H.pw <- function(t, inf, sup, lambda){  
  
  p1 <-  pmax(t-inf, 0) # 
  p2 <-  pmin(p1, sup-inf) #
  
  return(sum(lambda*p2)) #
  
}

# Ejemplo
H.pw(t=101, inf, sup, lambda)

# Que devuelve la función en este caso?

# plot
x <- min(inf):max(sup)
H <- rep(NA, length(x))

for (i in 1:length(x)){
  H[i] <- H.pw(x[i], inf, sup, lambda)
}
H
plot(x, H, type="l", lwd=3, col=2)

# Graficar la función de supervivencia
S <- 
plot(x, S, typ="l",lwd=3, col=2)

# Describir lo que se observa en el gráfico


# Ahora que tenemos nuestra función de riesgo acumulado, necesitamos
# definir la función para la cual vamos a encontrar la raiz 

f <- function(t, inf, sup, lambda, u){
  
  res <- H.pw(t, inf, sup, lambda) + # completar
  
  return(res)
}

# Ahora definimos la función que genera los tiempos de espera al evento
# Describir los pasos en la función:

root <- function(n, inf, sup, lambda){
  
  u <- runif(n) # 
  times <- rep(NA, n) 
  
  for(i in 1:n){
    result <- uniroot(f, interval=c(0, length(lambda)),
                      u=u[i], inf=inf, sup=sup, lambda=lambda) #
    times[i] <- result$root
  }
  return(times)
}

# Ahora generamos 10.000 tiempos de espera a la muerte y los guardamos
# en t
t <- root(10^4, inf, sup, lambda) #


# Para asegurarnos de que los resultados obtenidos en los pasos anteriores 
# son correctos, vamos a calcular una función de supervivencia a partir de los
# datos simulados y la vamos a comparar con la curva "teórica"

# Para esto vamos a usar el paquete "survival" que tiene una función "survfit" que
# nos va a calcular la función de supervivencia utilizando el estimador no paramétrico
# Kaplan-Meier de la función de Supervivencia

# Cargamos el paquete 
library(survival)

# survfit() necesita un objeto de tipo survival que se crea con la función
# Surv(). Esta función toma los tiempos y un indicador que indica cuando 
# hay evento = 1, o cuando no se observa el evento = 0
# En nuestro caso estamos asumiendo -de momento- que todas las observaciones experimentaron el evento
# es decir, no hay casos truncados.
eventos <- t < Inf

# Calculamos S
survival_fit <- survfit(Surv(t, eventos)~1)
# Extraemos los objetos necesarios para simular
sim_survival <- with(survival_fit, data.frame(time, surv))

# Graficamos
plot(, xlab="t", ylab="S(t)", xlim = c(0,101)) # completar

# Comparamos con la función de supervivencia teórica
lines(x, , lwd=3, col=2, lty=2) # completar

# Que se observa en el gráfico?





# Ejercicio:

# 2) Crear una función "ste" con argumentos (n, edades, lambda) para simular los
#    tiempos de espera a un evento a partir de una función de riesgo.
#    Agregar un argumento Haz que cuando T devuelva la función de riesgo acumulado H 
#    además de las t.





####################
#    VALIDACIÓN    #
####################
# Completar
n <- 10000
mort <- read.csv(file.path("datos","mx.csv"))
te <- ste(n, edades = mort$edad, lambda = mort$h, Haz = T)
eventos <-  
H <-  
# Grafica la curva de supervivencia simulada  
plot(survfit(Surv(te[[1]], eventos)~1), xlab="t", ylab="S(t)")
# Grafica la curva de supervivencia observada
lines(mort$edad, , lwd=3, col=2, lty=2)

fert <- read.csv(file.path("datos","fx.csv"))
te <- ste(n, edades = fert$edad, lambda = fert$h, Haz = T)
eventos <-  
H <- 
# Grafica la curva de supervivencia simulada
plot(survfit(Surv(te[[1]], eventos)~1), xlab="t", ylab="S(t)")
# Grafica la curva de supervivencia observada
lines(fert$edad-15, , lwd=3, col=2, lty=2)


