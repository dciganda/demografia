#install.packages("readxl")
library(readxl)

# Defunciones
def_raw <- # cargar datos defunciones
def_m <- # subset hombres
names(def_m)[1] <- "edad" 
str(def_m)
def_m <- # convertir todas las columnas a "numeric"
def_m[is.na(def_m)] <- # NAs a 0
def_m <- # hacer data.frame


def_f <- 
names(def_f)[1] <-  
str(def_f)
def_f <- 
def_f[is.na(def_f)] <- 
def_f <- 

# Años persona
exp_raw <- # cargar datos
exp_m <- # subset hombres
str(exp_m)
names(exp_m)[1] <- "edad"
exp_m$edad <- #columna edad a numeric
exp_m$edad[length(exp_m$edad)] <- 90

# mujeres
exp_f <- 
str(exp_f)
names(exp_f)[1] <- "edad"
exp_f$edad <- 
exp_f$edad[length(exp_f$edad)] <- 90


# Intervalo abierto 90+ defunciones
open_int_m <- # seleccionar la filas y columnas correspondientes y hacer una suma por columnas con apply()  
open_int_f <-  

def_m <- rbind(def_m[def_m$edad %in% 0:89,], c(90, open_int_m))
def_f <- rbind(def_f[def_f$edad %in% 0:89,], c(90, open_int_f))
  
# Obtener las tasas 
nMx_m <-  # HOMBRES

nMx_f <-  # MUJERES

# Formatear los datos para el plot
nMx_m <- reshape(nMx_m,
                 direction = "long",
                 varying = list(names(nMx_m)),
                 v.names = "Male",
                 timevar = "Year",
                 times = 1996:2020)

nMx_m <- cbind(def_m[,1], nMx_m[,1:2])
names(nMx_m)[1] <- "Age" 

nMx_f <- reshape(nMx_f,
                 direction = "long",
                 varying = list(names(nMx_f)),
                 v.names = "Female",
                 timevar = "Year",
                 times = 1996:2020)

nMx_f <- cbind(def_f[,1], nMx_f[,1:2])
names(nMx_f)[1] <- "Age" 


# graficar 
library(ggplot2)

plot_Mx <- function(dat, anios, sex, edades, smooth = F,
                    spar_val = 0.3, log_escale = T, return_data = F, as_list =F, save = F){
  dat[dat$Age == "110+", ] <- "110"
  dat[,2] <- as.numeric(dat[,2])
  dat <- dat[dat$Age %in% edades & dat$Year %in% anios, c("Year","Age",sex)] 
  dat[,1] <- as.numeric(dat[,1])
  dat[,3] <- as.numeric(dat[,3])
  
  
  if(log_escale){
    dat[,3] <- log(dat[,3])
    ylims <- c(-13, 1)
  }else{ylims <- c(0, 1)}
  
  if(smooth){
    split_dat <- split(dat, dat$Year)
    
    sm_y <- lapply(split_dat, function(x) smooth.spline(x[,3], spar = spar_val)$y)
    
    dat <- cbind(dat[,1:2], unlist(sm_y))
    
  }
  
  names(dat) <- c("Year","Age","Mx")
  
  p <- ggplot(dat, aes(x = Age, y = Mx,
                       group = as.factor(Year),
                       colour = Year))+
    geom_line() +  scale_colour_gradient(low = "orange", high = "red")+
    theme_bw() +
    ylim(ylims)+
    ylab("M(x)") + xlab("Edad")+
    theme(legend.position = c(0.85, 0.3),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  if(save){
    pdf(file.path("..","..","..","imagenes", "asfr.pdf"), width=6, height=6) 
    print(p)
    dev.off()
  }
  
  print(p)
  
  if(return_data){
    if(as_list){
      
      split_dat <-  split(dat, dat$Year)
      Mx_list <- lapply(split_dat, function(x) x[,3])  
      return(Mx_list)
    }else{
      return(dat[,3])}
  }
}

# eliminar anio 1996 

# definir función compute_lt

# obtener tasas con función plot_Mx

# calcular la esperanza de vida al nacer para cada serie de Mx con compute_lt y lapply

# graficar
