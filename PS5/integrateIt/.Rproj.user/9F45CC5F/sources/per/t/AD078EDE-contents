#' Integral approximation
#'
#' Estimates the integral of a function in specified boundaries
#'
#' @param n Number of segments to estimate the integral. Must be even if using the Simpson method.
#' @param f The function you are wanting to estimate the integral of
#' @param ab The bounds you are integrating between.
#' @param rule Which rule you want to use for estimation; Trapezoid or Simpson.
#'
#' @return A list with the elements
#'  \item{Object}{The Trapezoid or Simpson integral object}
#'  \item{coordinates}{The x and y coordinates the integral was estimated using}
#'  \item{result}{The solution to the integration estimate}
#' @author Jordon Newton
#' @note This is a rudimentary implementation of two estimation methods.
#' @examples
#'
#' n <- 10
#' f <- function(x){return(x^2)}
#' ab <- c(0,10)
#'
#' integrateIt(n,f,ab,"Trapezoid")
#' integrateIt(n,f,ab,"Simpson")
#'
#'
#' @import utils
#' @rdname integrateIt
#' @export

#Set the generic for the integration
setGeneric(name = "integrateIt",
           def=function(n,f,ab,rule)
             {standardGeneric("integrateIt")}
)

#Define the method to actually integrate, using a function, number of steps, start and end point, and rule specification
setMethod(f = "integrateIt",
          definition = function(n,f,ab,rule=c("Trapezoid","Simpson")){
            #calculate the size of the step
            h=(ab[2]-ab[1])/n
            #Calculate the x coordinates
            xvals <- c(seq(ab[1],ab[2],h))
            #use the x coordinates and function to get the y coordinates
            yvals <- c(f(xvals))
            #Set the trapezoid function
            if(rule=="Trapezoid"){
              #Use head/tail to get the (1,2,2,...,2,1) pattern
              result = h/2*sum(head(yvals,length(xvals)-1)+tail(yvals,length(xvals)-1))
              #Store the coordinates
              coordinates <- cbind(xvals,yvals)
              #Define the new trapezoid
              class_set <- new("Trapezoid", n = n, f=f, ab = ab, rule = "Trapezoid", result = result, coordinates = coordinates)
            }
            #Do the same, except with simpson
            if(rule=="Simpson"){
              #Create a coeficient vector to mirror the (1,4,2,4,2,...2,4,1) pattern
              coefs <- c(1,rep(c(4,2), times = (n-2)/2),4,1)
              #calculate the result
              result = h/3*sum(coefs*yvals)
              #Store the coordinates
              coordinates <- cbind(xvals,yvals)
              #set the class
              class_set <- new("Simpson", n = n, f=f, ab = ab, rule = "Trapezoid", result = result, coordinates = coordinates)
            }
            #create a list to return
            returnlist <- list("Integral_object"=class_set,
                               "coordinates" = coordinates,
                               "result" = result)
            #return the list
            return(returnlist)
            }

)

