library(ggplot2)

plot_fx <- function(dat, points = F, ...){
  fx <- table(factor(floor(dat$edad), levels = 10:50)) / max(dat$id)
  if(points){
    points(10:50, fx, ...)
  }else{
    plot(10:50, fx, ...)
  }
}


plot_fx_hfd <- function(dat, cohorts, type,
                        legend = TRUE, save = FALSE) {
  # Data preprocessing
  dat$Age[dat$Age == "12-"] <- "12"
  dat$Age[dat$Age == "55+"] <- "55"
  dat[, 2] <- as.numeric(dat[, 2])
  dat <- dat[dat$Cohort %in% cohorts, c(1, 2, 3)]
  names(dat) <- c("Cohort", "Age", "fx")
  dat$fx[dat$fx == "."] <- "0"
  dat$fx <- as.numeric(dat$fx)
  
  # Set up the plot
  plot.new()
  plot.window(xlim = range(dat$Age), ylim = range(dat$fx))
  
  # Create color gradient
  cohorts_numeric <- as.numeric(as.character(dat$Cohort))
  min_cohort <- min(cohorts_numeric)
  max_cohort <- max(cohorts_numeric)
  cohort_colors <- colorRampPalette(c("orange", "red"))(length(unique(dat$Cohort)))
  
  # Plot data
  for (i in seq_along(unique(dat$Cohort))) {
    cohort <- unique(dat$Cohort)[i]
    cohort_data <- dat[dat$Cohort == cohort, ]
    if (type == "points") {
      points(cohort_data$Age, cohort_data$fx, col = cohort_colors[i], pch = 16, cex = 1)
    } else {
      lines(cohort_data$Age, cohort_data$fx, col = cohort_colors[i])
    }
  }
  
  # Add axes and labels
  axis(1)
  axis(2)
  title(xlab = "Age", ylab = "f(x)")
  
  # Add a box around the plot
  box()
  
  
  if(length(unique(dat$Cohort)) == 1){
    # Simple legend for single cohort
    legend("topright", legend = unique(dat$Cohort), 
           col = cohort_colors, 
           pch = if(type == "points") 16 else NULL,
           lty = if(type != "points") 1 else NULL,
           title = "Cohort", cex = 0.9, bty = "n")
  } else {
    # Gradient legend for multiple cohorts
    legend_image <- as.raster(matrix(rev(cohort_colors), ncol = 1))
    rasterImage(legend_image, xleft = par("usr")[2] * 0.75, 
                ybottom = par("usr")[4] * 0.4, 
                xright = par("usr")[2] * 0.7725, 
                ytop = par("usr")[4] * 0.8)
    text(x = par("usr")[2] * 0.80, y = par("usr")[4] * 0.79, labels = max_cohort)
    text(x = par("usr")[2] * 0.80, y = par("usr")[4] * 0.41, labels = min_cohort)
    text(x = par("usr")[2] * 0.77, y = par("usr")[4] * 0.84, labels = "Cohort", adj = 0.5)
  }
  
  
  # Save the plot if requested
  if (save) {
    pdf(file.path("..", "..", "..", "imagenes", "asfr.pdf"), width = 6, height = 6)
    par(bg = "transparent")
    plot_fx_hfd(dat, cohorts, type, legend = legend)
    dev.off()
  }
}

# plot_fx_hfd <- function(dat, cohorts, type,
#                         legend = TRUE, save = FALSE) {
#   # Data preprocessing
#   dat$Age[dat$Age == "12-"] <- "12"
#   dat$Age[dat$Age == "55+"] <- "55"
#   dat[, 2] <- as.numeric(dat[, 2])
#   dat <- dat[dat$Cohort %in% cohorts, c(1, 2, 3)]
#   names(dat) <- c("Cohort", "Age", "fx")
#   dat$fx[dat$fx == "."] <- "0"
#   dat$fx <- as.numeric(dat$fx)
#   
#   # Set up the plot
#   plot.new()
#   plot.window(xlim = range(dat$Age), ylim = range(dat$fx))
#   
#   # Create color gradient
#   cohorts_numeric <- as.numeric(as.character(dat$Cohort))
#   min_cohort <- min(cohorts_numeric)
#   max_cohort <- max(cohorts_numeric)
#   cohort_colors <- colorRampPalette(c("orange", "red"))(length(unique(dat$Cohort)))
#   
#   # Plot data
#   for (i in seq_along(unique(dat$Cohort))) {
#     cohort <- unique(dat$Cohort)[i]
#     cohort_data <- dat[dat$Cohort == cohort, ]
#     if (type == "points") {
#       points(cohort_data$Age, cohort_data$fx, col = cohort_colors[i], pch = 16, cex = 1)
#     } else {
#       lines(cohort_data$Age, cohort_data$fx, col = cohort_colors[i])
#     }
#   }
#   
#   # Add axes and labels
#   axis(1)
#   axis(2)
#   title(xlab = "Age", ylab = "f(x)")
#   
#   # Add a box around the plot
#   box()
#   
#   if(legend){
#     # Add gradient legend
#     legend_image <- as.raster(matrix(rev(cohort_colors), ncol = 1))
#     rasterImage(legend_image, xleft = par("usr")[2] * 0.75, 
#                 ybottom = par("usr")[4] * 0.4, 
#                 xright = par("usr")[2] * 0.7725, 
#                 ytop = par("usr")[4] * 0.8)
#     text(x = par("usr")[2] * 0.80, y = par("usr")[4] * 0.79, labels = max_cohort)
#     text(x = par("usr")[2] * 0.80, y = par("usr")[4] * 0.4, labels = min_cohort)
#     text(x = par("usr")[2] * 0.77, y = par("usr")[4] * 0.84, labels = "Cohort", adj = 0.5)
#   }
#   
#   # Save the plot if requested
#   if (save) {
#     pdf(file.path("..", "..", "..", "imagenes", "asfr.pdf"), width = 6, height = 6)
#     par(bg = "transparent")
#     plot_cohort_asfr(dat, cohorts, type)
#     dev.off()
#   }
# }


