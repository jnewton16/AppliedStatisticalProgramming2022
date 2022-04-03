integrateIt <- function(n, f, ab, rule = "Trapezoid"){
  if(length(ab)!=2){stop("The starting/ending points are not a vector of length 2")}
  h=(ab[2]-ab[1])/n
  xvals <- c(seq(ab[1],ab[2],h))
  yvals <- c(f(xvals))
  if(rule=="Trapezoid"){
    result = h/2*sum(head(yvals,length(xvals)-1)+tail(yvals,length(xvals)-1))
  }
  if(rule=="Simpson"){
    result = h/3
  }
  coordinates <- cbind(xvals,yvals)
  returnlist <- list("coordinates" = coordinates, "result" = result)
  return(returnlist)
}


