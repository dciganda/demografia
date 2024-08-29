################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# 3er Laboratorio: Proceso Reproductivo y fecundidad                           # 
# 29 de Agosto de 2024                                                         #   
################################################################################

# cargar datos de Huteritas
fx_ht <- read.csv(file.path("datos", "asfrs_ht.csv"), header = T) 

# En el laboratorio anterior comenzamos a trabajar con un modelo del proceso
# reproductivo para una cohorte, es decir, un modelo que simula las
# trayectorias reproductivas de una cohorte de mujeres desde el matrimonio
# hasta el comienzo de la menopausia

# Repasamos el funcionamiento del modelo y los resultados preliminares: 

gen_hst <- function(n, fi, ns, mu, su){
  
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12 # 
  
  wt_c <- lapply(1:50, function(x) rnbinom(n, 1, fi)) #
  
  wt_b <- list()
  wt_b[[1]] <- wt_u + wt_c[[1]] + 9 #
  
  hst <- list()
  hst[[1]] <- as.data.frame(cbind(id = 1:n,
                                  edad = wt_b[[1]]/12,
                                  paridad = 1))
  
  for(i in 2:50){
    
    wt_b[[i]] <- wt_b[[i-1]] + ns + wt_c[[i]] + 9 #
    
    nid <- which(wt_b[[i]]>50*12) #
    
    wt_b[[i]][nid] <- NA 
    
    if(sum(is.na(wt_b[[i]])) == n){break} #
    
    hst[[i]] <- as.data.frame(cbind(id = rep(1:n,i),
                                    edad = unlist(wt_b)/12,
                                    paridad = rep(1:i, each = n)))
    hst[[i]] <- hst[[i]][!is.na(hst[[i]]$edad),]
  }
  
  return(hst)
  
}

# Simulamos las trayectorias reproductivas de una cohorte de 
# 1000 mujeres 

# n mayor
ls_hst <- gen_hst(n = 1000, fi = 0.2, ns = 6, mu = 18, su = 1.1)

# Graficamos las tasas específicas de fecundidad por edad f(x) teniendo a
# las f(x) de los Huteritas como referencia
plot(fx_ht, col = "violet", pch = 16, ylim = c(0,0.7))


# extraemos el ultimo elemento de la lista con los outputs del modelo
hst <- ls_hst[[length(ls_hst)]]

# Calculamos las tasas espeificas de fecundidad por edad
fx <- table(factor(floor(hst$edad), levels = 10:50)) / max(hst$id)

#graficamos
points(10:50, as.data.frame(fx)[,2], col = "red")

# Las tasas específicas de fecundidad por edad que obtenemos del modelo
# Siguen el patrón esperado en las primeras edades, pero no se observa 
# la caída característica a partir de determinada edad donde se alcanza un máximo


################################################################  
# Modelos del proceso reproductivo - Modelo con fecundabilidad #
# dependiente de la edad                                       #  
################################################################

# Para generar una distribución realista de f(x) tenemos que considerar 
# como evoluciona la capacidad biológica de concebir en el tiempo (edad)

# Para aproximar como la fecundabilidad cambia en función de la edad vamos
# a utilizar el modelo derivado por Coale and Trussell (1974) 

df <- as.data.frame(cbind(age = c(10,25,30,35,40,49)*12,
                          series= c(100,94,86,70,36,5)/100))
plot(df, lwd = 3, cex = 1.3,
     ylab = "Fecundidad Relativa",
     xlab = "Edad en Meses",
     ylim = c(0,1))
legend(460, 0.9, legend = "Modelo de Coale & Trussell",
       lwd = c(1), col = "black",
       cex=0.95, bty = "n",
       lty = 0,
       pch = 1,
       y.intersp = 1,
       x.intersp = 0.5)

# Necesitamos obtener un modelo paramétrico para representar el cambio 
# de fi en t y poder utilizarlo en nuestro modelo de trayectorias reproductivas

# Podemos aproximarnos con una una función logística: 1/(1 + exp(-r*(x-x0)))
# Definimos visualmente unos valores de los parámetros que producen una curva que 
# se ajusta al modelo de Coale y Trussell

x0_ini <- 38*12      # punto de inflexión ("ini"  = valor inicial aproximado) 
r_ini  <- 0.022      # tasa
age <- seq(10*12, 50*12,1)

lines(age, 1 / (1 + exp(r_ini*(age-x0_ini))), col = "red", lwd = 2)

# Describir brevemente lo que se observa en el gráfico 


# Podemos estimar los parámetros de la función logística con la ayuda de la
# función nls() que implementa un algoritmo de mínimos cuadrados no-lineales
# (nonlinear least-squares)

nls_fit <- nls(df$series ~ 1 / (1 + exp(r*(age-x0))),
               data = df,
               start = list(r = 0.022, x0 = 38*12)) # valores iniciales

# accedemos a los valores de los parámetros estimados
r <- coef(nls_fit)[1] 
x0 <- coef(nls_fit)[2]

# Ejercicio: graficar una línea sobre el gráfico anterior con 
# la función logística definida por los parámetros estimados en el paso anterior.




# Ahora que tenemos un modelo para fi_t podemos incorporarlo a nuestro modelo 
# del proceso reproductivo

################################################
# Modelo con fecundabilidad dependiente de t   #
################################################

# Ejercicio: Completar la descripción en las líneas indicadas

gen_hst_t <- function(n, ns, x0, r, mu, su){
  
  id = vector()
  wt_c = vector()
  
  meses <- 1:(50*12) # 
  
  # fecundabilidad
  fi_t <- 0.2 / (1 + exp(r*(meses-x0))) # 
  fi_t[max(meses)] <- 0 #  
  #plot(meses,fi_t)
  
  wt_u <- rlnorm(n, log(mu^2/ sqrt(mu^2+su^2)), sqrt(log(1 + su^2/mu^2))) * 12 # 

  
  fi_it <- lapply(1:n, function(x) fi_t[wt_u[x]:length(fi_t)]) # 
  
  maxt <- max(sapply(fi_it, length)) # 
  
  for(t in 1:maxt){ 
    
    is <- which(runif(n) < sapply(fi_it, function(x) x[t])) # 
    
    if(length(is)!=0){ 
      
      wts <- sapply(is, function(x) (wt_u[x]-1) + t) # 
      
      id <- c(id, is) 
      wt_c <- c(wt_c, wts)
      
      
      fi_it[is] <- lapply(fi_it[is], function(x){x[t:(t+9+ns)] <- NA; return(x)}) # 
      
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


# Ejercicio: Obtener las tásas específicas de fecundidad por edad para una cohorte de
# 5000 mujeres con un perído de no-suceptibilidad de 6 meses, edad media a la union de
# 20 años (y desvío estandar 1.1) utilizando el modelo con fecundabilidad
# dependiente de t), graficar.

hst_t <- # completar

# graficamos sobre los resultados anterirores
plot(fx_ht, col = "violet", pch = 16, ylim = c(0,0.7))

hst <- ls_hst[[length(ls_hst)]]
fx <- table(factor(floor(hst$edad), levels = 10:50)) / max(hst$id)
points(10:50, as.data.frame(fx)[,2], col = "red")

fx_t <- table(factor(floor(hst_t$edad), levels = 10:50)) / max(hst_t$id)
points(10:50, as.data.frame(fx_t)[,2], col = "blue")

# Describir lo que se observa en el gráfico. 
# A qué se deben las diferencias entre los modelos?
# A que se deben las diferencias entre el modelo más reciente
# y los datos de los Huteritas?



# Ejercicio:

# Buscar una combinación de parámetros que genere unas f(x) simuladas similares
# a los datos de la cohorte de Huteritas.

hst_ht_sim <- # completar

# graficamos sobre los resultados anteriores
plot(fx_ht, col = "violet", pch = 16, ylim = c(0,0.7))
fx_ht_sim <- table(factor(floor(hst_ht_sim$edad), levels = 10:50)) / max(hst_ht_sim$id)
points(10:50, as.data.frame(fx_ht_sim)[,2], col = "red")
