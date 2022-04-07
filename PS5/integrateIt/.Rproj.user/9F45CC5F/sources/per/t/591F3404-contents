#' Integral approximation classes
#'
#' Class of type "Trapezoid" or of type "Simpson"
#'
#' Integral objects have the following slots:
#' \itemize{
#' \item \code{n} The number of segments of integration
#' \item \code{f} The function being integrated
#' \item \code{ab} A vector containing the start and end point of integration
#' \item \code{rule} A specification of which integration estimation rule to use
#' }
#' @aliases Trapezoid/Simpson-class, Trapezoid/Simpson-method, integrateIt/print
#' @author Jordon Newton
#' @import methods
#' @rdname integrate_it_classes
#' @export

#Put the trapezoid class together with the 4 pieces needed, the calculated result, and the coordinates used
setClass(Class="Trapezoid",
         representation = representation(
           n = "numeric",
           f = "function",
           ab = "numeric",
           rule = "character",
           result = "numeric",
           coordinates = "matrix"
         ),
         prototype = prototype(
            n = numeric(),
            f = function(x) {return(x)},
            ab = numeric(),
            rule = character(),
            result = numeric(),
            coordinates = matrix()
         )
)

#' @export
#Do the same for the simpson class
setClass(Class="Simpson",
           representation = representation(
             n = "numeric",
             f = "function",
             ab = "numeric",
             rule = "character",
             result = "numeric",
             coordinates = "matrix"
           ),
           prototype = prototype(
             n = numeric(),
             f = function(x) {return(x)},
             ab = numeric(),
             rule = character(),
             result = numeric(),
             coordinates = matrix()
           )
)

#' @export
#Set some validity screens on trapezoid
setValidity("Trapezoid", function(object){
  #test ab to make sure it is corerct length
  ab_test <- length(object@ab)==2
  if(!ab_test){stop("The vector containing a and b contains the wrong number of values; make sure it only contains a and b.")}

  #Test the output of the function
  func_test <- is.numeric(object@f(object@ab))
  if(!func_test){stop("Y must return a numeric value.")}
  }
)

#' @export
#Set validity screen on simpson
setValidity("Simpson",function(object){
#Test to make sure the simpson rule works, as it requires an even n
  if(object@n%%2==1){return("Simpson only work with an even number of points")}

  #test ab to make sure it is corerct length
  ab_test <- length(object@ab)==2
  if(!ab_test){stop("The vector containing a and b contains the wrong number of values; make sure it only contains a and b.")}

  #Test the output of the function
  func_test <- is.numeric(object@f(object@ab))
  if(!func_test){stop("Y must return a numeric value.")}

  }
)

#' @export
#initialize
setMethod("initialize", "Trapezoid",
          function(.Object,...){
            value=callNextMethod()
            return(value)
          }
)
#initialize
#' @export
setMethod("initialize", "Simpson",
          function(.Object,...){
            value=callNextMethod()
            return(value)
          }
)

#' @export
#Define the print function for trapezoid; thanks to Alma's code for helping me fix an issue
setMethod(f = "print",
          signature(x = "Trapezoid"),
          definition = function(x){
            print(x@result)
          }
        )

#' @export
#Define the print function for simpson
setMethod(f = "print",
          signature(x = "Simpson"),
          definition = function(x){
            print(x@result)
          }
)
