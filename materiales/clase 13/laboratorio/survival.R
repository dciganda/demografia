################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Simulación de tiempos a distintos eventos demográficos          # 
# 31 de Octubre de 2024                                                        #   
################################################################################

# El objetivo de este módulo es simular tiempos de espera o duraciones a los
# eventos fallecimiento y primer hijo. Para esto partimos de unas tasas 
# específicas de mortalidad y unas tasas específicas de transición a la maternidad. 
# Se asume que el riesgo de los eventos es *constante* en cada intervalo (edades)
# Bajo este supuesto, las tasas de ocurrencia/exposición representan al riesgo del evento.
# Es decir que el vector de tasas específicas representa la función de riesgo del evento.

# Vamos a empezar por entender las relaciones entre las funciones de riesgo, riesgo 
# acumulado y función de supervivencia.

###############
#   DATOS     #
###############
# Cargamos los datos de las tasas específicas por edad del primer hijo  
# *condicionales*  de una cohorte
fert <- read.csv(file.path("datos","fx.csv"))
plot(fert)

# Describir lo que se observa en el gráfico. Que son estos valores? Que hay en 
# numerador y denominador para su cálculo?

# Cargamos los datos de las tasas específicas de mortalidad de una cohorte 
mort <- read.csv(file.path("datos","mx.csv"))
plot(mort)

# Describir lo que se observa en el gráfico- Que son estos valores? Que hay en 
# numerador y denominador para su cálculo?


# Estas tasas van a representar nuestra función de riesgo constante a intervalos para 
# las variables aleatorias "Tiempo al Fallecimiento" y "Tiempo al Primer Hijo" 

#################################################################################
#   Función de riesgo, función de riesgo acumulado y función de supervivencia   #
#################################################################################

# input
x <- mort$edad
lambda <- mort$h

# Responder:
# Cuál es el riesgo de morir:
# 1) Entre edades 0-1

# 2) Entre edades 50-51

# 3) Entre edades 100-101


# Cual es el riesgo de morir:
# 1) antes de alcanzar la edad exacta 0

# 2) antes de alcanzar la edad exacta 1

# 3) antes de alcanzar la edad exacta 100

# 4) antes de alcanzar la edad exacta 101

# Ejercicio:
# Crear una función pw_H (piece-wise Hazard) con argumentos x, lambda
# que calcule el riesgo acumulado hasta desde edad 0 hasta la edad maxima
# en los datos

# función de riesgo acumulado


# Responder utilizando la función: 
# Cual es el riesgo de morir:
# 1) antes de alcanzar la edad exacta 0

# 2) antes de alcanzar la edad exacta 1

# 3) antes de alcanzar la edad exacta 100

# 4) antes de alcanzar la edad exacta 101

# Ejercicio: Graficar la función de riesgo acumulado.
# (computar la función en un vector H utilizando pw_H)


# Ejercicio: Computar y graficar la función de supervivencia

# Responder: 
# Cual es la probabilidad de sobrevivir:
# 1) a edad exacta 0

# 2) a edad exacta 1

# 3) a edad exacta 100

# 4) a edad exacta 101

# Que significa que esta última probabilidad no sea 0?

