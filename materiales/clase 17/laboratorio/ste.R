ste <- function(n, edades, lambda, Haz = F){
  
  inf <- seq(0, length(edades)-1,1)
  sup <- seq(1, length(edades))
  
  H.pw <- function(t, inf, sup, lambda){  
    p1 <-  pmax((t-inf), 0)
    p2 <-  pmin(p1, sup-inf)
    return(sum(lambda*p2)) 
  }
  
  H <- rep(NA, length(edades))
  x <- min(inf):max(sup)

  for (i in 1:length(x)){
    H[i] <- H.pw(x[i], inf, sup, lambda)
  }
  
  f <- function(t, inferior, superior, nivel, u){
    return(H.pw(t, inf=inferior, sup=superior, lambda=nivel) + log(u))
  }
  
  ne <- exp(-H)[length(exp(-H))]
  nn <- n - sum(runif(n) < ne)
  
  if(nn == 0){
    
    t <- rep(Inf, n)
    
    return(t); break()
    
  }
  
  root <- function(nn, inf, sup, lambda){
    u <- runif(nn, min=exp(-H)[length(exp(-H))])
    times <- rep(NA, nn)
    for(i in 1:nn){
      result <- uniroot(f, interval=c(0, length(lambda)),
                        u=u[i], inferior=inf, superior=sup, nivel=lambda)
      times[i] <- result$root
    }
    return(times)
  }
  
  t_e <- root(nn, inf, sup, lambda)
  
  if(n-length(t_e)!=0){
    
    t <- sample(c(t_e, rep(Inf, n-length(t_e))))
    
  }else{
    
    t <- t_e
    
  }
  
  if(min(edades)!=0){ 
    t <- t + min(edades)  
  }
  
  if(Haz){ return(list(t, H)) }else{ return(t) }
  
}
