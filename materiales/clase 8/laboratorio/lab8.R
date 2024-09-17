################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 8vo Laboratorio: Proceso Reproductivo y fecundidad                           # 
# 17 de Septiembre de 2024                                                     #   
################################################################################

source("plot_fun.R")

###############################################################################
# Nacimientos No Deseados                                                     #
###############################################################################
# Ejercicio: El objetivo ahora es agregar información sobre el tipo de 
# nacimiento en el modelo.
# Para entender la relación entre nacimientos deseados, no deseados y totales. 

# Eercicio: completar en los lugares indicados en el modelo que se presenta
# abajo y luego obtener las graficas para las fx totales, las fx que sólo
# cuentan nacimientos deseados y las fx que sólo cuentan naciemientos no 
# deseados 

gen_hst_nd <- function(n = 5000,
                       ns = 10,
                       x0 = 430,
                       r = 0.02,
                       mu = 25,
                       su = 3.9,
                       md = 2,
                       sd = 1.4,
                       c = 0.03){
  
  id <-  vector()
  wt_c <- vector()
  k <- rep(0,n)
  edad <- 1:600
  
  # evolucion de la probabilidad de concebir con la edad
  fi_t <- 0.2 / (1 + exp(r*(edad-x0)))
  fi_t[589:600] <- 0
  
  # tiempo de espera a la unión
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12 
  
  # probabilidad de concebir luego de la union para cada mujer
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)]) 
  
  # número deseado de hijos
  dk <- round(rlnorm(n, meanlog = log(md^2/ sqrt(md^2+sd^2)),
                     sdlog = sqrt(log(1 + sd^2/md^2))),0) 
  
  maxt <- max(sapply(fi_it, length))
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it,`[`, t) * c^(k >= dk))   
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t)  
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) 
      
      k <- as.vector(table(factor(id, levels = 1:n))) # COMPLETAR
      
    }
    
  }
  
  # data
  hst <- as.data.frame(cbind(id = id, wt_c = wt_c))
  hst <- hst[order(hst$id),]
  hst <- as.data.frame(cbind(id = hst$id,
                             edad = (hst$wt_c + 9)/12,
                             nac = rep(table(hst$id), table(hst$id)),
                             paridad = sequence(table(hst$id))))
  
  hst$nd <- #completar
    
    return(hst)
  
}


# Obtener resultados y graficar las distintas tasas (negro para fx,
# azul d_fx y rojo nd_fx)
# Describir los resultados obtenidos
out <- gen_hst_nd()

fx <- plot_fx(dat = out, return_fx = T)
d_fx <- plot_fx(, return_fx = T, lines = T, col = "blue")
nd_fx <- plot_fx(, return_fx = T, lines = T, col = "red")


# Reducir la eficacia de los anticonceptivos y comparar resultados



