#' PoisMLE
#'
#' The class represents a poisson fitted model
#'
#' Fitted model objects have the following slots:
#' \itemize{
#' \item \code{y} The original data
#' \item \code{MLE} The maximum likelihood estimator for the dataset
#' \item \code{LL} The log likelihood calculated from the observed data assuming the MLE is correct
#' \item \code{SE} The standard error for the MLE
#' \item \code{SEtype} The method used to calculate the standard error
#' }
#'
#' @aliases PoissonMLE
#' @author Jordon Newton
#' @rdname poisMLE
#' @export


#Put together the class definition
setClass(Class="PoisMLE",
         representation = representation(
           y = "numeric",
           MLE = "function",
           LL = "numeric",
           SE = "numeric",
           SEtype = "character"
         ),
         prototype = prototype(
           y = numeric(),
           MLE = function(x) {return(x)},
           LL = numeric(),
           SE = numeric(),
           SEtype = character()
         )
        )
