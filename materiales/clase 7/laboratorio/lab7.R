################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 7mo Laboratorio: Proceso Reproductivo y fecundidad                           # 
# 12 de Septiembre de 2024                                                     #   
################################################################################

source("plot_fun.R")

# Continuamos trabajando con el modelo del proceso reproductivo en contexto de
# fecundidad regulada definido en el laboratorio anterior

# modelo
gen_hst_d <- function(n, ns, x0, r, mu, su, md, sd, c){
  
  id <-  vector()
  wt_c <- vector()
  k <- rep(0,n)
  edad <- 1:600
  
  # fecundabilidad
  fi_t <- 0.2 / (1 + exp(r*(edad-x0)))
  fi_t[589:600] <- 0
  #plot(edad,fi_t)
  
  # tiempo de espera a la unión
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12 #
  
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)]) #
  
  dk <- round(rlnorm(n, meanlog = log(md^2/ sqrt(md^2+sd^2)),
                     sdlog = sqrt(log(1 + sd^2/md^2))),0) #

  maxt <- max(sapply(fi_it, length))
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it,`[`, t) * c^(k >= dk)) #  
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t) # 
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) #
      
      k <- as.vector(table(factor(id, levels = 1:n))) #
      
    }
    
  }
  
  # data
  hst <- as.data.frame(cbind(id = id, wt_c = wt_c))
  hst <- hst[order(hst$id),]
  hst <- as.data.frame(cbind(id = hst$id,
                             edad = (hst$wt_c + 9)/12,
                             nac = rep(table(hst$id), table(hst$id)),
                             paridad = sequence(table(hst$id))))
  return(hst)
}

# Cargamos los datos de España
fx_es <- read.table(file.path("datos","asfr_hfd.txt"), skip = 2, header=T,
                    stringsAsFactors = F)

################################################################################
# Análisis del efecto de las preferencias y prácticas anticonceptivas          #  
################################################################################
# El objetivo ahora es realizar un análisis similar al que hicimos con el 
# modelo anterior al explorar el efecto de la variación de la edad media a la
# union y el período de no suceptibilidad. Pero esta vez vamos a explorar el 
# efecto de las preferencias (nr deseado de hijos) y las prácticas
# anticonceptivas.

# la función "plot_fx" está definida en "plot_fun.R" y devuelve por default
# la sumatoria de las fx (además del gráfico)
# la función "plot_fx_hfd" prepara y gráfica datos provenientes de la
# Human Fertility Database

# Vamos a tomar como referencia empírica las fx de la cohorte de nacidas 
# en 1940 en España

plot_fx_hfd(dat = fx_es, cohort = 1940, type = "points")

# Simular las trayectorias reproductivas para la combinación de parámetros
# que ajusta bien a los datos

hst_ref <- 

# Graficar y guardar las tasa global de fecundidad simulada en "tfr_ref"
tfr_ref <- plot_fx(hst_ref, lines  = T)

#######################################################################  
# ANALISIS DE SENSIBILIDAD                                            #
#######################################################################
# El objetivo ahora es observar el efecto sobre las tasas específicas
# de fecundidad y sobre el número promedio de hijos por mujer
# de la variación en el valor de los parámetros


#################################
# Prácticas Anticonceptivas     #
#################################
c_vals <- seq(0.01, 0.5, 0.05)

# Lista con datos para cada uno de los valores de c
hst_c_var <- 

# graficamos y guardamos la tasa global de fecundidad de la población referencia
ref_tfr_c <- 

# graficamos y guardamos la tasa global de fecundidad correspondiente
# a los distintos valores de c
colors <- rainbow(length(c_vals))
var_tfr_c <- lapply(1:length(c_vals), function(x) plot_fx(dat = hst_c_var[[x]],
                                                              lines = T,
                                                              col = colors[x]))
add_legend(c_vals, colors)

# obtenemos las diferencia en la tasa global de fecundidad (TFR) 
tfr_dif_c <- 

# graficamos
plot(c_vals, tfr_dif_c)

# Describir los resultados obtenidos.




#################################
# Media del nr deseado de hijos #
#################################
md_vals <- seq(1, 5, 0.5)

hst_md_var <- 

ref_tfr_md <- 

colors <- rainbow(length(md_vals))
var_tfr_md <- 
  
add_legend(md_vals, colors)

tfr_dif_md <- 

plot(md_vals, tfr_dif_md)

# Describir los resultados obtenidos.




##################################
# Desvío del nr deseado de hijos #
##################################
sd_vals <- seq(0.5, 5, 0.5)
hst_sd_var <- 

ref_tfr_sd <- 

colors <- rainbow(length(sd_vals))
var_tfr_sd <- 
  
add_legend(sd_vals, colors)

tfr_dif_sd <- 

plot(sd_vals, tfr_dif_sd)

