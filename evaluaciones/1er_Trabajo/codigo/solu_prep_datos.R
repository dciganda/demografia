library(readxl)

# Defunciones
def_raw <- as.data.frame(read_excel("datos/def.xlsx", skip = 14))
def_m <- def_raw[1:115, 3:(ncol(def_raw)-1)]
names(def_m)[1] <- "edad" 
str(def_m)
def_m <- apply(def_m, 2, as.numeric)
def_m[is.na(def_m)] <- 0
def_m <- as.data.frame(def_m)


def_f <- def_raw[119:236, 3:(ncol(def_raw)-1)]
names(def_f)[1] <- "edad" 
str(def_f)
def_f <- apply(def_f, 2, as.numeric)
def_f[is.na(def_f)] <- 0
def_f <- as.data.frame(def_f)

# Años persona
exp_raw <- as.data.frame(read_excel("datos/Total_pais_poblacion_por_sexo_y_edad_1996-2050.xls", skip = 4))

exp_m <- exp_raw[99:189, 1:26]
str(exp_m)
names(exp_m)[1] <- "edad"
exp_m$edad <- as.numeric(exp_m$edad)
exp_m$edad[length(exp_m$edad)] <- 90

# mujeres
exp_f <- exp_raw[193:283, 1:26]
str(exp_f)
names(exp_f)[1] <- "edad"
exp_f$edad <- as.numeric(exp_f$edad)
exp_f$edad[length(exp_f$edad)] <- 90


# Intervalo abierto 90+ defunciones
open_int_m <- apply(def_m[def_m$edad >= 90,2:ncol(def_m)], 2, sum, na.rm = T)  
open_int_f <- apply(def_f[def_f$edad >= 90,2:ncol(def_f)], 2, sum, na.rm = T)  

def_m <- rbind(def_m[def_m$edad %in% 0:89,], c(90, open_int_m))
def_f <- rbind(def_f[def_f$edad %in% 0:89,], c(90, open_int_f))
  
# Obtener las tasas 
nMx_m <- def_m[,-1]/exp_m[,-1] # HOMBRES

nMx_f <- def_f[,-1]/exp_f[,-1] # MUJERES

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


plot_Mx(dat = nMx_m, anios = 1996:2020,edades = 0:90, sex = "Male")
plot_Mx(dat = nMx_f, anios = 1996:2020,edades = 0:90, sex = "Female")

# eliminar anio 1996 

nMx_m <- nMx_m[nMx_m$Year>1996,]
plot_Mx(dat = nMx_m, anios = 1997:2020,edades = 0:90, sex = "Male")
nMx_f <- nMx_f[nMx_f$Year>1996,]
plot_Mx(dat = nMx_f, anios = 1997:2020,edades = 0:90, sex = "Female")


compute_lt <- function(nMx, x, sex, tabla = TRUE){
  
  # Definimos el número de intervalos
  nmax <- length(nMx)
  
  # Definimos los factores de separación nax 
  
  # creamos un vector vacio para guardar los nax
  nax <- vector()
  
  get_na0 <- function(nMx, sex){
    
    if(sex ==  "M"){
      
      if (nMx[1] < 0.0023){
        
        na0 <- 0.14929 - 1.99545 * nMx[1]
      }else{
        
        if(nMx[1] >= 0.0023 & nMx[1] < .08307){
          
          na0 <- .02832 + 3.26021 * nMx[1]
          
        }else{
          
          na0 <- 0.29915
        }
      }
    }
    if(sex == "F"){
      
      if (nMx[1] <  0.01724){
        
        na0 <- .14903 - 2.05527 * nMx[1]
      }else{
        
        if(nMx[1] >= 0.01724 & nMx[1] < 0.06891){
          
          na0 <- 0.04667 + 3.88089 * nMx[1]
          
        }else{
          
          na0 <- .31411
        }
      }  
      
    }
    
    return(na0)
    
  }
  
  # definimos a0 con la ayuda de la función "get_na0"
  na0 <- get_na0(nMx, sex)
  
  # assignamos los factores a cada intervalo
  nax[1] <- na0
  nax[2:nmax] <- 1/2
  
  # convertimos las nMx en nqx
  nqx <- (1 * nMx)/(1 + (1 - nax) * nMx)
  
  # nos aseguramos que la probabilidad en el último intervalo sea 1
  nqx[nmax] <- 1
  
  # Construimos las lx
  lx <- cumprod(c(1, 1 - nqx))
  
  # creamos un vector con los sobrevivientes en x+n
  lxn <- lx[-1]
  
  # Obtenemos las defunciones
  ndx <- -diff(lx)
  
  # Obtenemos los años persona en el intervalo nLx
  nLx <- lxn + ndx * nax
  
  nLx[nmax] <- lxn[nmax-1]/nMx[length(nMx)]
  
  # Calculamos los años persona por encima de x
  Tx <- rev(cumsum(rev(nLx)))
  
  # Calculamos la esperanza de vida a edad x
  ex <- Tx/lx[1:nmax]
  
  # Creamos la tabla
  lt <- data.frame(x, nax = round(nax, 4),
                   nMx = round(nMx,4),
                   nqx = round(nqx[1:nmax], 4), lx = round(lx[1:nmax],4),
                   ndx = round(ndx, 4), nLx = round(nLx, 4), Tx = round(Tx, 
                                                                        2), ex = round(ex, 2))
  
  if(tabla){
    return(lt)  
  }else{
    return(ex[1])
  }
  
  
}

nMxc_m <- plot_Mx(dat = nMx_m, anios = 1997:2020, edades = 0:90, sex = "Male", return_data = T, as_list = T, log_escale = F)
nMxc_f <- plot_Mx(dat = nMx_f, anios = 1997:2020, edades = 0:90, sex = "Female", return_data = T, as_list = T, log_escale = F)


exc_M <- lapply(nMxc_m, function(x) compute_lt(x, x = 0:90, sex = "M", tabla = F))
exc_F <- lapply(nMxc_f, function(x) compute_lt(x, x = 0:90, sex = "F", tabla = F))


plot(1997:2020, exc_F, ylim = c(70, 82))
points(1997:2020, exc_M, col = "red")
