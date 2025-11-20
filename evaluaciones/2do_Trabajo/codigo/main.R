################################################################################
# Demografía 2025 IESTA                                                        #
# Segundo Trabajo                                                              # 
################################################################################
###########################
# Segunda parte           #
###########################

# Cargar datos de tamaño de población 
pop <- 
# Cargar datos de tasas específicas de fecundidad por edad de periodo (1x1)
f <- 
# Cargar tablas de mortalidad de período (1x1) para hombres y mujeres  
Lf <- 
Lm <- 
  
# Funcion para preparar los datos
data_prep <- function(pop, f, Lf, Lm, ini_year, save = F){
  
  all_dat <- lapply(list(pop, f, Lf, Lm), function(x) x[x$Year >= ini_year,])
  
  last_pop <- all_dat[[1]][all_dat[[1]]$Year == max(all_dat[[1]]$Year),  c("Year","Age", "Female","Male")]
  last_pop[last_pop$Age == "110+", "Age"] <- "110"
  last_pop$Age <- as.numeric(last_pop$Age)
  
  all_dat[[1]] <- all_dat[[1]][all_dat[[1]]$Year == ini_year,  c("Age", "Female","Male")]
  
  all_dat[[2]][all_dat[[2]]$Age == "12-", "Age"] <- "12"
  all_dat[[2]][all_dat[[2]]$Age == "55+", "Age"] <- "55"
  all_dat[[2]][,2] <- as.numeric(all_dat[[2]][,2])
  
  all_dat[[3]][all_dat[[3]]$Age == "110+", "Age"] <- "110"
  all_dat[[4]][all_dat[[4]]$Age == "110+", "Age"] <- "110"
  all_dat[[3]][,2] <- as.numeric(all_dat[[3]][,"Age"])
  all_dat[[4]][,2] <- as.numeric(all_dat[[4]][,"Age"])
  
  
  all_dat[[2]] <- reshape(all_dat[[2]],
                          direction = "wide",
                          timevar = "Year",
                          idvar = "Age")
  
  all_dat[[3]] <- reshape(all_dat[[3]][,c("Year","Age","Lx")],
                          direction = "wide",
                          timevar = "Year",
                          idvar = "Age")
  
  all_dat[[4]] <- reshape(all_dat[[4]][,c("Year","Age","Lx")],
                          direction = "wide",
                          timevar = "Year",
                          idvar = "Age")
  
  if(save){
    
    write.table(all_dat[[1]], "datos/pop.txt")
    write.table(all_dat[[2]], "datos/asfrs.txt")
    write.table(all_dat[[3]], "datos/Lf.txt")
    write.table(all_dat[[4]], "datos/Lm.txt")
    write.table(last_pop, "datos/last_pop.txt")
    
    
  }
  
  return(all_dat)
  
}

# seleccionar primer año con datos comunes
ini_year <- max(min(pop$Year), min(f$Year),
                min(Lf$Year), min(Lm$Year))

# crear lista con datos
dat <- data_prep(pop, f, Lf, Lm, ini_year, save = T)

# Extraer:
# Población inicial femenina
Nf <- dat[[1]][,"Female"]
# Población inicial masculina
Nm <- dat[[1]][,"Male"]
# tasas de fecundidad
f <- rep(0, length(Nm))
f <- dat[[2]][,-1]
# Años persona mujeres y hombres
Lf <- dat[[3]][,-1]
Lm <- dat[[4]][,-1]


# función para preparar los datos para el gráfico pirámide
reshape_long <- function(dat, yr, int){
  dat <- as.data.frame(dat)
  maxage <- nrow(dat) - 1
  names(dat) <- c(rbind((paste0("f_", yr)), paste0("m_", yr)))
  
  dat0 <- reshape(data = dat,
                  varying = names(dat),
                  direction = "long",
                  sep = "_")
  names(dat0) <- c("yr", "count", "count")
  ages_lon <- rbind(dat0[,1:2], dat0[,c(1,3)])
  ages_lon$group <- c(rep("females", nrow(dat0)), rep("males", nrow(dat0)))
  ages_lon$age <- rep(seq(0, maxage, int), 2)
  totals_v <- apply(dat, 2, sum)
  totals <- (c(totals_v[c(TRUE, FALSE)], totals_v[c(FALSE, TRUE)]))
  ages_lon$totals <- rep(totals, each = length(seq(0, maxage, int)))
  ages_lon$pct <- ages_lon$count / ages_lon$totals
  
  ages_pyr <- ages_lon
  ages_pyr$pct[ages_pyr$group == "males"] <- -ages_lon$pct[ages_lon$group == "males"]
  
  return(ages_pyr)
}

# gráfico pirámide
pd_plot <- function(dat, int, pt = FALSE, country){
  bly_palette <- c("#009E73", "#D55E00")
  
  # Split data into males and females
  females <- subset(dat, group == 'females')
  males <- subset(dat, group == 'males')
  
  # Prepare data
  ages <- females$age
  pct_females <- females$pct
  pct_males <- males$pct
  
  max_pct <- max(abs(dat$pct), na.rm = TRUE)
  
  # Set up plotting area with reversed y-axis
  par(mar = c(5, 5, 4, 2) + 0.1)
  plot(NULL, xlim = c(-max_pct, max_pct), ylim = c(min(ages) - int/2, max(ages) + int/2),
       xlab = "Percent of Population", ylab = "Age",
       main = paste("Estructura por Edad de la Población de", country, ":", unique(dat$yr)),
       axes = FALSE)
  
  axis(1, at = seq(-max_pct, max_pct, length.out = 5), labels = abs(seq(-max_pct, max_pct, length.out = 5)))
  axis(2, at = ages, labels = ages, las = 2)
  box()
  
  # Draw bars for females and males
  half_int <- int / 2
  for (i in seq_along(ages)){
    rect(0, ages[i] - half_int, pct_females[i], ages[i] + half_int, col = bly_palette[1], border = NA)
    rect(0, ages[i] - half_int, pct_males[i], ages[i] + half_int, col = bly_palette[2], border = NA)
  }
  
  # Add legend
  legend("topright", legend = c("Females", "Males"), fill = bly_palette, bty = "n")
}

# matriz de Leslie modificada
leslie <- function(L, m, int) {
  n <- length(L)
  M <- matrix(0, n, n)
  
  if(length(L) != length(m)){
    m_aux <- rep(0, length(L))
    m_aux[13:56] <- m
    m <- m_aux
  }
  
  # Survival rates
  px <- exp(diff(log(L)))
  diag(M[-1, -ncol(M)]) <- px
  M[n, n - 1] <- M[n, n] <- L[n] / (L[n - 1] + L[n])
  M[is.na(M)] <- 0
  
  # Fertility rates
  for(i in 1:(n - 1)) {
    if(m[i] != 0 | m[i + 1] != 0) {
      M[1, i] <- (m[i] + m[i + 1] * L[i + 1] / L[i]) * int / 2
    }
  }
  return(M)
}

# Función para proyectar la población con condiciones de fecundidad y mortalidad
# cambiantes
p_pop_t <- function(country, f, Lf, Lm, Nf, Nm, iter, int, srb, ini_year){
 
}

iter <- 130
sw_pop <- p_pop_t(country = "",
                  f = f,
                  Lf = Lf/100000,
                  Lm = Lm/100000,
                  Nf = Nf,
                  Nm = Nm,
                  iter = iter,
                  int = 1,
                  srb = 1.05,
                  ini_year = ini_year)


# cargar datos más reciente de estructura por edad y sexo de la población
# (hay que crear el archivo last_pop primero o hacerlo de otra manera)
lp <- read.table("datos/last_pop.txt", header = T) 
names(lp) <- c("year","age", "female", "male")

# graficar estructura de la población del último año disponible
lpp <- reshape_long(dat = lp[3:4], yr = 2022, int = 1)
pd_plot(dat = lpp,int = 1,country = "Suecia")

# graficar estructura de la población proyectada
pd_plot(dat = reshape_long(dat = sw_pop[,c(ncol(sw_pop)-1, ncol(sw_pop))],
                           yr = 2022, int = 1),int= 1,
        country = "Suecia")

