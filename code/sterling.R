# sterling function:
sterling <- function(n,g){
  
  gfact <- 1
  for(i in 1:g){
    gfact <- i*gfact
  }
  gfact
  
  
  x <- (1/gfact)
  
  b <- 0
  for(w in 1:g){
    
    
    ifact <- 1
    for(s in 1:w){
      ifact <- s*ifact
    }
    
    
    gifact <- 1
    for(p in 0:(g-w)){
      gifact <- (p+1)*gifact
    }
    
    
    b <- b + ( (gfact/(gifact*ifact)) * (((-1)^(g-w))*(w)^(n))  ) 
    
  }
  
  m <- x*b
  m
}



# Example:
sterling(25,10)
