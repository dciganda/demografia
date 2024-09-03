######################### ######################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 4to Laboratorio: Proceso Reproductivo y fecundidad                           # 
# 3 de Septiembre de 2024                                                      #   
################################################################################

# Modelo con fecundabilidad dependiente de t
gen_hst_t <- function(n, ns, x0, r, mu, su){
  
  id = vector()
  wt_c = vector()
  
  meses <- 1:(50*12) # vector con el tiempo t en meses
  
  # fecundabilidad
  fi_t <- 0.2 / (1 + exp(r*(meses-x0))) # fi en t
  fi_t[max(meses)] <- 0 # nos aseguramos que haya riesgo 0 luego de 50 años de edad 
  #plot(meses,fi_t)
  
  # tiempo de espera a la unión
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12
  #plot(prop.table(table(round(rlnorm(10^6, meanlog = log(mu*12), sdlog = log(1.1)),0))))
  
  # Tomamos la fecundabilidad en los meses luego del mes a la union para cada mujer en n
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)])
  
  # tomamos el máximo nr de meses por los que vamos a observar a las mujeres en nuestra muestra
  maxt <- max(sapply(fi_it, length))
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it, function(x) x[t])) # que mujeres experimentan una concepción en el mes t 
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t) # tiempo de espera a la concepción
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      # eliminamos el riesgo desde la concepción hasta el final del periodo de no susceptibilidad
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) 
      
    }
    
  }
  
  data <- as.data.frame(cbind(id = id, wt_c = wt_c))
  data <- data[order(data$id),]
  hst <- as.data.frame(cbind(id = data$id,
                             edad = (data$wt_c + 9)/12,
                             nac = rep(table(data$id), table(data$id)),
                             paridad = sequence(table(data$id))))
  return(hst)
}

#######################################################################  
# Modelos del proceso reproductivo - Efectos de los determinantes     #
# próximos en la fecundidad natural                                   #
#######################################################################

# El objetivo ahora es utilizar el modelo para estimar el efecto de distintos
# factores en la fecundidad de una cohorte. 

# Desde el punto de vista del análisis del modelo este ejercicio se conoce como
# "analisis de sensibilidad" en el que se busca estimar la contribución
# relativa de cada uno de los parámteros en la explicación de la variabilidad
# en el resultado.


# Empecemos por fijar una población de referencia sobre la que vamos a medir
# las diferencias.
# Para esto utilizamos la parametrización que obtuvimos al intentar ajustar 
# las tasas de los Huteritas.
# El objetivo es obtener las tasas específicas y la fecundidad total en esta
# población de referencia tomando el tamañano de la cohorte = 5000.
# Para esto vamos utilizar una función para graficar los resultados y obtener
# la fecundiad total asociada a cada parametrización del modelo.
# Esto nos va permitir calcular las diferencias en la fecundidad total en el
# análisis de sensibilidad.

hst_ref <- # completar

plot_sum_fx <- function(dat, lines = F, ...){
  fx <- # completar
  if(lines){
    lines(10:50, fx, ...)
  }else{
    plot(10:50, fx, ...)
  }
  return(sum(fx))
}

plot_sum_fx(hst_ref, ylim = c(0,0.7))


# El objetivo ahora es observar el efecto de un incremento de:
# 1 mes en el período de no suceptibilidad
# 6 meses en la edad media a la union

# Variando los parámetros de 1 por vez y dejando el resto fijos

ns_vals <- seq(6, 24, 1)

hst_ns_var <- # completar, utilizando lapply() 

ref_tfr <- plot_sum_fx(hst_ref, ylim = c(0, 0.7))
var_tfr_ns <- lapply(hst_ns_var, plot_sum_fx, lines = T, col = "red")

# Describir lo que se observa en el grafico


# Ahora analizamos las diferencias en la Ttasa global de fecundidad
tfr_dif_ns <- sapply(var_tfr_ns, function(x) x - ref_tfr)

plot(ns_vals, tfr_dif_ns)

# Describir lo que se observa en el grafico


# Ejercicio: Realizar el mismo análisis, esta vez explorando el efecto de la 
# edad media a la union en el rango 18 - 25 y del punto de inflexión
# en el rango de edades 25 a 35 - Describir los resultados


