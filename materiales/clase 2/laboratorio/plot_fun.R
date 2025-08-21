# Function to create alpha-transparent colors
alpha <- function(col, alpha) {
  rgb(t(col2rgb(col)/255), alpha=alpha)
}

plot_asfr <- function(dat, pch = 19, cex = 1.3, alpha = 0.7, grid_rows = 25,
                      margin_y = 0.05, margin_x = 0.04, each_x = 2, each_y = 0.02,
                      x_coord = 37, y_coord = 0.5) {
  col <- rgb(131/255, 75/255, 159/255)
  
    draw_grid <- function(x_coords, y_coords){
    for(i in seq_along(x_coords)){
      segments(x0 = x_coords[i], x1 = x_coords[i],
               y0 = min(y_coords), y1 = max(y_coords), col=alpha(rgb(0,0,0), 0.1))
    }
    for(i in seq_along(y_coords)){
      segments(x0 = min(x_coords), x1 = max(x_coords),
               y0 = y_coords[i], y1 = y_coords[i], col=alpha(rgb(0,0,0), 0.1))
    }  
  }
  
  add_empty_plot_with_grid <- function(x_data, y_data, x_label_text, y_label_text){
    xmargin <- (max(x_data, na.rm = TRUE) - min(x_data, na.rm = TRUE)) * margin_x
    ymargin <- (max(y_data, na.rm = TRUE) - min(y_data, na.rm = TRUE)) * margin_y + 0.025
    
    new_x <- round(seq(min(x_data, na.rm = TRUE) - xmargin,
                       max(x_data, na.rm = TRUE) + xmargin, by = each_x), 0)
    new_y <- round(seq(0,
                       max(y_data, na.rm = TRUE) + ymargin, by = each_y), 2)
    
    plot(x_data, y_data, xlab=x_label_text, ylab= y_label_text,
         xlim = c(min(new_x), max(new_x)),
         ylim = c(min(new_y), max(new_y)),
         type = 'n', bty = "n", axes = FALSE)
    
    axis(side=1, at=round(new_x,0))
    
    ticks <- round(new_y,1)
    axis(side=2, at=ticks)
    
    draw_grid(new_x, new_y)
    
    par(new=TRUE)
    
    return(list(new_x = new_x, new_y = new_y))
  }
  
  plot_dat <- function() {
    par(bg = "white")
    
    grid_result <- add_empty_plot_with_grid(x_data = dat$age, y_data = dat$fx,
                                            x_label_text = "Edad",
                                            y_label_text =  "f(x)")
    
    ticks <- seq(min(dat$fx), max(dat$fx), by = 0.1)
    axis(side = 2, at = ticks)
    
    points(dat$age, dat$fx, pch = pch, col = alpha(col, alpha), cex = cex)
    
    op <- par(family = "sans")
    legend(x_coord, y_coord, 
           legend = paste0("Tasas Especificas \n de Fecundidad \n Cohorte Huteritas"),
           lwd = 1, col = alpha(col, alpha), lty = 0,
           pch = 16,
           cex = 1.2, bty = "n",
           y.intersp = 0.8,
           x.intersp = 0.5)
    
    par(op)
  }
  
  plot_dat()
  
 
}

plot_hst <- function(dat, ylim, n, legend = FALSE) {
  
  plot(dat$edad, dat$id, type = 'n', xlim = c(0, 60), ylim = ylim,
       xlab = "Edad", ylab = "", axes = FALSE)
  
  axis(1)
  
  unique_paridades <- sort(unique(dat$paridad))
  color_gradient <- colorRampPalette(c("orange", "red"))
  colors <- color_gradient(length(unique_paridades))
  
  for (i in 1:n) {
    segments(10, i, 50, i, col = "black")
  }
  
  points(dat$edad, dat$id, col = colors[match(dat$paridad, unique_paridades)],
         pch = 19, cex = 1)
  
  if (legend) {
    # Create color gradient for legend
    legend_colors <- rev(color_gradient(100))
    
    # Define legend position and size
    legend_x <- par("usr")[2] * 0.85  # x position
    legend_y <- par("usr")[4] * 0.8  # y position
    legend_width <- par("usr")[2] * 0.02  # width
    legend_height <- par("usr")[4] * 0.25  # height
    
    # Draw color gradient box
    rasterImage(as.raster(matrix(legend_colors, ncol=1)), 
                legend_x, legend_y - legend_height, 
                legend_x + legend_width, legend_y,
                interpolate = TRUE)
    
    # Add box around the color gradient
    rect(legend_x, legend_y - legend_height, 
         legend_x + legend_width, legend_y)
    
    # Add text labels
    text(legend_x + 0.55, legend_y + legend_height * 0.3, "Paridad", pos = 1, cex = 0.95)
    text(legend_x + legend_width * 1.2, legend_y, max(unique_paridades), pos = 4, cex = 0.85)
    text(legend_x + legend_width * 1.2, legend_y - legend_height, min(unique_paridades), pos = 4, cex = 0.85)
    
    # Add middle value label
    middle_paridad <- round(mean(range(unique_paridades)))
    text(legend_x + legend_width * 1.2, legend_y - legend_height/2, middle_paridad, pos = 4, cex = 0.85)
  }
}

plot_cum_fec <- function(dat, margin_y = 0.25, each_x = 2, each_y = 0.1,
                         grid_rows = 25, alpha = 0.7) {
  
  col <- rgb(131/255, 75/255, 159/255)
  
  draw_grid <- function(x_coords, y_coords){
    for(i in seq_along(x_coords)){
      segments(x0 = x_coords[i], x1 = x_coords[i],
               y0 = min(y_coords), y1 = max(y_coords), col=alpha(rgb(0,0,0), 0.1))
    }
    for(i in seq_along(y_coords)){
      segments(x0 = min(x_coords), x1 = max(x_coords),
               y0 = y_coords[i], y1 = y_coords[i], col=alpha(rgb(0,0,0), 0.1))
    }  
  }
  
  add_empty_plot_with_grid <- function(x_data, y_data, x_label_text, y_label_text){
    new_x <- seq(0, 60, by = each_x)
    max_y <- max(y_data, na.rm = TRUE) + margin_y
    new_y <- seq(0, max_y, by = each_y)
    
    plot(x_data, y_data, xlab = x_label_text, ylab = y_label_text,
         xlim = c(0, 60),
         ylim = c(0, max_y),
         type = 'n', bty = "n", axes = FALSE)
    
    axis(side = 1, at = new_x)
    axis(side = 2, at = new_y)
    
    draw_grid(new_x, new_y)
    
    par(new = TRUE)
    
    return(list(new_x = new_x, new_y = new_y))
  }
  
  plot_dat <- function() {
    par(bg = "white")
    
    grid_result <- add_empty_plot_with_grid(x_data = dat$edad, y_data = dat$cum_fec,
                                            x_label_text = "Edad",
                                            y_label_text = "Fecundidad Acumulada")
    
    points(dat$edad, dat$cum_fec, pch = 19, col = alpha(col, alpha), cex = 1)
  }
  
  plot_dat()

}

plot_fx <- function(dat) {
  edad <- 0:50
  dat <- merge(dat, as.data.frame(edad), by = "edad", all = TRUE)
  dat$cum_nac <- cumsum(!is.na(dat$id))
  dat$cum_fec <- dat$cum_nac / max(dat$id, na.rm = TRUE)
  inferior <- dat[dat$edad %in% seq(10, 49, 1), c("edad", "cum_fec")]
  superior <- dat[dat$edad %in% seq(11, 50, 1), c("edad", "cum_fec")]
  tasas_edad <- superior$cum_fec - inferior$cum_fec 
  plot(10:49, tasas_edad, xlab = "Edad", ylab = "Tasas")
  cat(paste("TFR:", sum(tasas_edad)), "\n")
}