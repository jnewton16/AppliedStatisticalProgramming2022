#' PoisMLE
#'
#' The class represents a poisson fitted model
#'
#' Fitted model objects have the following slots:
#' \itemize{
#' \item \code{y} The original data. Given it is a poisson model, all data must be positive integers or 0.
#' \item \code{MLE} The maximum likelihood estimator for the dataset
#' \item \code{LL} The log likelihood calculated from the observed data assuming the MLE is correct
#' \item \code{SE} The standard error for the MLE
#' \item \code{SEtype} The method used to calculate the standard error
#' }
#'
#' @aliases PoissonMLE
#' @author Jordon Newton
#' @rdname poisMLE
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{mle}}
#' @seealso \code{\link{standardError}}
#' @export


#Put together the class definition
setClass(Class="PoisMLE",
         representation = representation(
           y = "numeric",
           MLE = "numeric",
           LL = "numeric",
           SE = "numeric",
           SEtype = "character"
         ),
         prototype = prototype(
           y = numeric(),
           MLE = numeric(),
           LL = numeric(),
           SE = numeric(),
           SEtype = character()
         )
        )

#' @export
#Set up validity checks
setValidity("PoisMLE",function(object){
  #Test the data y to see if any of y is not a positive integer or 0
  positive <- all(sapply(y, function(x){ifelse(x>0,T,F)}))
  if(!positive){stop("The observations must all be positive")}

  integer <- all(sapply(y, function(x){ifelse(x%%1==0,T,F)}))
  if(!integer){stop("The observations must all be integers")}
})

#' @export
#initializer
setMethod("initialize","PoisMLE",
          function(.Object,...){
            value=callNextMethod()
            return(value)
          }
)
