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

plot_sum_fx <- function(dat, lines = F, ylims){
  edad <- 0:50
  dat <- merge(dat, as.data.frame(edad), by = "edad", all = T)
  dat$cum_nac <- cumsum(!is.na(dat$id))
  dat$cum_fec <- dat$cum_nac/max(dat$id, na.rm = T)
  inferior <- dat[dat$edad %in% seq(10,49,1),c("edad", "cum_fec")]
  superior <- dat[dat$edad %in% seq(11,50,1),c("edad", "cum_fec")]
  tasas_edad <- superior$cum_fec - inferior$cum_fec 
  if(lines){
    lines(10:49, tasas_edad, col = "red")
  }else{
    plot(10:49, tasas_edad, ylim = ylims)
  }
  return(sum(tasas_edad))
}

plot_asfr <- function(dat, pch = 19, cex = 1.3,
                     alpha = 0.7, yl = 0.2, save = F){
  
  
  col <- rgb(131/255,75/255,159/255)
  
  save_path <- file.path("..","..","imagenes")
  
  save_plot <- function(name){
    p <- recordPlot()
    pdf(file.path(save_path, paste0(name,".pdf")), width=7, height=7) 
    print(p)
    dev.off()
  }
  
  add_empty_plot_with_grid <- function(x_data, y_data, x_label_text, y_label_text, grid_rows = 25){
    
    draw_grid <-function(x_coords, y_coords, grid_rows){
      
      for(i in 1:grid_rows){
        
        # we draw the i_th vertical line
        segments(x0 = x_coords[i], x1 = x_coords[i],
                 y0 = min(y_coords), y1 = max(y_coords), col=alpha(rgb(0,0,0), 0.1))
        
        # we draw the i_th horizontal line
        segments(x0 = min(x_coords), x1 = max(x_coords),
                 y0 = y_coords[i], y1 = y_coords[i], col=alpha(rgb(0,0,0), 0.1))
        
        
      }
    }
    
    x_coords <- seq(min(x_data), max(x_data), length.out = grid_rows)
    y_coords <- seq(min(y_data), max(y_data) + yl, length.out = grid_rows)
    
    plot(x_data, y_data, xlab=x_label_text, ylab= y_label_text,
         xlim = c(min(x_data),max(x_data)), ylim = c(min(y_data),max(y_data)+ yl),
         type = 'n', bty = "n", axes = F)
    
    # specify x-axis interval
    axis(side=1, at=seq(min(x_data), max(x_data), by = 5))
    
    draw_grid(x_coords, y_coords, grid_rows)
    
    par(new=TRUE)
  }
  
  plot_dat <- function(){
    
    # change background color
    par(bg = "white")
    
    add_empty_plot_with_grid(dat$age, dat$fx, "Edad", "f(x)")
    
    ticks <- seq(min(dat$fx), max(dat$fx) + yl, by = 0.1)
    axis(side=2, at=ticks)
    
    # Add points to plot
    points(dat$age, dat$fx, pch = pch, col = alpha(col, alpha), cex = cex, bty = "n")
    
    # Legend
    op <- par(family = "sans")
    legend(37.5, 0.65, legend = paste0("Tasas Especificas de Fecundidad \n Cohorte Huteritas"),
           lwd = c(1), col = alpha(col, alpha), lty = c(0),
           pch = c(16),
           cex=1, bty = "n",
           y.intersp = 0.8,
           x.intersp = 0.5)
    
    ## reset plotting parameters
    par(op)
    
    if(save){
      save_plot("asfr_ht")
    }
  }
  
  plot_dat()
  
}

plot_hst <- function(dat, ylim, n, save = F, legend = T){
  p <- ggplot(dat, aes(x = edad, y = id)) +
    geom_point(aes(colour = paridad), size = 2)+
    scale_colour_gradient(low = "orange", high = "red")+
    xlim(c(0,60)) + 
    ylim(c(ylim)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))+
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"))+
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank())+
    ylab(NULL) + xlab("Edad")
  for (i in 1:n) {p <- p + geom_segment(x=10, y=i, xend=50, yend=i)}
  if(save){
    pdf(file.path("..","..","imagenes", "rep_lines.pdf"), width=4, height=4) 
    print(p + theme(legend.position = "none"))
    dev.off()
  }
  if(!legend){
    p <- p + theme(legend.position = "none")
  }
  return(p)
}
plot_cum_fec <- function(dat, ylim, n, save = F){
  p <- ggplot(dat, aes(x = edad, y = cum_fec)) +
    geom_point(size = 2)+ 
    xlim(c(0,60)) + 
    ylim(c(ylim)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    ylab("Fecundidad Acumulada") + xlab("Edad")
  if(save){
    pdf(file.path("..","..","imagenes", "cum_fec.pdf"), width=4, height=4) 
    print(p)
    dev.off()
  }
  p
}
plot_fx <- function(dat, points = F, col = "black"){
  edad <- 0:50
  dat <- merge(dat, as.data.frame(edad), by = "edad", all = T)
  dat$cum_nac <- cumsum(!is.na(dat$id))
  dat$cum_fec <- dat$cum_nac/max(dat$id, na.rm = T)
  inferior <- dat[dat$edad %in% seq(10,49,1),c("edad", "cum_fec")]
  superior <- dat[dat$edad %in% seq(11,50,1),c("edad", "cum_fec")]
  tasas_edad <- superior$cum_fec - inferior$cum_fec 
  if(points){
    points(10:49, tasas_edad, lwd = 1, col = col)
  }else{
    plot(10:49, tasas_edad)
  }
  cat(paste("TFR:", sum(tasas_edad)), "\n")
}

plot_cohort_asfr <- function(dat, anios, type, save = F){
  dat[dat$Age == "12-", "Age"] <- "12"
  dat[dat$Age == "55+", "Age"] <- "55"
  dat[,2] <- as.numeric(dat[,2])
  dat <- dat[dat$Cohort %in% anios, c(1,2,3)]  
  names(dat) <- c("Cohorte","Edad","fx")
  dat[,3] <- as.numeric(dat[,3])
  
  p <- ggplot(dat, aes(x = Edad, y = fx,
                       group = as.factor(Cohorte),
                       colour = Cohorte))
  if(type=="points"){
    p <- p + geom_point(size = 2)    
  }else{
    p <- p + geom_line()
  }
  p <- p +  scale_colour_gradient(low = "orange", high = "red")+
    theme_bw() +
    ylab("f(x)") + xlab("Edad")+
    theme(legend.position = c(0.85, 0.7),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) +
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  if(save){
    pdf(file.path("..","..","..","imagenes", "asfr.pdf"), width=6, height=6) 
    print(p)
    dev.off()
  }
  p
}

plot_cohort_asfr_base <- function(dat, anios, type, save = F){
  dat[dat$Age == "12-", "Age"] <- "12"
  dat[dat$Age == "55+", "Age"] <- "55"
  dat[,2] <- as.numeric(dat[,2])
  dat <- dat[dat$Cohort %in% anios, c(1,2,3)]  
  names(dat) <- c("Cohorte","Edad","fx")
  dat[,3] <- as.numeric(dat[,3])
  
  plot(dat$Edad, dat$fx, col = "violet")
  
}
