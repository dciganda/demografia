################################################################################
# Demografía 2024 IESTA                                                        #
# Daniel Ciganda                                                               # 
# Laboratorio: Simulación de tiempos a distintos eventos demográficos          # 
# 31 de Octubre de 2024                                                        #   
################################################################################

# El muestreo por transformada inversa es una técnica potente para generar
# números aleatorios a partir de una distribución de probabilidad deseada
# utilizando su función de distribución acumulada (CDF). 
# Los pasos para implementar este método son los siguientes:

# 1. Calcular la CDF F(x) de la distribución deseada.
# 2. Generar una muestra aleatoria U de la distribución uniforme U(0,1).
# 3. Encontrar X tal que F(X) = U, es decir, encontrar la CDF inversa F^-1(U).

# PDF de la distribución exponencial :
# lambda * exp(-lambda * x)

# CDF de la distribución exponencial:
# 1 - exp(-lambda * x)

# Inversa de la CDF de la distribución exponencial:
# -log(1 - u) / lambda

# Ejercicio: Crear una función para generar valores aleatorios de la
# distribución exponencial

generate_exponential <- function(n, lambda) {

}

# Testear la función:
lambda <- 1
n <- 10^6
samples <- 

# Comparar con la función de r
true_samples <- rexp(n, rate=lambda)

# Graficar
# Crear histogramas
hist(samples, breaks = 50, freq = FALSE, xlim = c(0, 12),
     main = "Comparación de Histogramas y Densidad Teórica",
     xlab = "Valor", ylab = "Densidad", col = rgb(0.2, 0.8, 0.5, 0.5),
     border = "white")
hist(true_samples, breaks = 50, freq = FALSE, xlim = c(0, 12),
     add = TRUE, col = rgb(0.8, 0.2, 0.5, 0.5), border = "white")

# Superponer la densidad teórica
curve(dexp(x, rate = lambda), from = 0, to = 12, add = TRUE,
      col = "black", lwd = 2, lty = 2)

legend("topright", legend = c("Transformada Inversa", "Función R",
                              "Densidad Teórica"),
       fill = c(rgb(0.2, 0.8, 0.5, 0.5), rgb(0.8, 0.2, 0.5, 0.5), NA),
       border = "white", lty = c(NA, NA, 2), lwd = c(NA, NA, 2),
       col = c(NA, NA, "black"))