################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Proyecciones de Población                                                    # 
# 28 de Noviembre de 2024                                                      #   
################################################################################
# El objetivo ahora es incorporar a nuestra proyección la población masculina.
# Par esto vamos a modificar la matriz de Leslie para que devuelva los 
# nacimientos *totales* en el primer elemento del vector que resulta de
# multiplicar la matriz por la población en un período dado.

# Cargamos los datos
swf <- read.table("datos/sweden_data_1993_females.txt", header=F)
names(swf) <- c("age", "p93", "L", "f")
swm <- read.table("datos/sweden_data_1993_males.txt", header=F)
names(swm) <- c("age", "p93", "L")

# guardamos columnas en objetos
f <- swf$f
Lf <- swf$L
Lm <- swm$L
Nf <- swf$p93
Nm <- swm$p93

# Modificamos la matriz para que devuelva los nacimientos totales

leslie <- function(L, m) {

}

# Proyectamos la población usando la matriz (en el próximo paso corregimos la 
# primera entrada)
Mlf <- 
swf$p98 <- 
  
# Guardamos los nacimientos en un objeto
births <- 

# Corregir la primera entrada para que contenga la población de mujeres en el
# primer grupo de edad en 1998
swf$p98[1] <- 
  
# Proyectar la población masculina
Mlm <- 
swm$p98 <-

swm$p98[1] <- 

# Ejercicio:
# Crear una función "p_pop" que proyecte la población por edad y sexo
# tomando los siguientes argumentos:
# las tasas específicas de fecundidad "f"
# los años persona "Lf" y "Lm"
# un vector incial de población femenina "Nf" y  masculina "Nm"
# un parametro "iter" con el número de iteraciones / intervalos de proyección
# y la tasa de masculinidad "srb"


p_pop <- function(f, Lf, Lm, Nf, Nm, iter, int, srb){
  
  # Construir una matriz para hombres y otra para mujeres
  
  
  # crear una matriz "pop" para guardar la población base y las proyecciones
  
  
  # projectar la población de hombres y mujeres
  is <- seq(1,by=2, len=iter)
  
  for(i in is){
    
    # Proyectar usando la matriz
  
      
    # guardar nacimientos
    
    
    # calcular población en 1er grupo de edad   
    
    
  }
  
  return(pop)
  
}

sw_pop <- p_pop(f = f,
                Lf = Lf/100000,
                Lm = Lm/100000,
                Nf = Nf,
                Nm = Nm,
                iter = 2,
                int = 5,
                srb = 1.05)









