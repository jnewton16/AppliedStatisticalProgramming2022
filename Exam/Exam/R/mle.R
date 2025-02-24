#' maximum likelihood estimator
#'
#' Calculates the maximum likelihood estimator for lambda in a poisson distribution
#'
#' @param y The vector of observed data to calculate the log likelihood from
#'
#' @return A single numeric output:
#'  \item{MLE}{The maximum likelihood estimate of lamda}
#' @author Jordon Newton
#' @note This uses the function for a poisson distribution. Do not use with data from another distribution.
#' @examples
#'
#' set.seed(1625)
#' y <- rpois(1000,80)
#' mle(y)
#'
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{standardError}}
#' @aliases MLE
#' @aliases maximumlikelihoodestimator
#' @rdname mle
#' @export

#set the generic for the mle
setGeneric(name = "mle",
           def=function(y)
           {standardGeneric("mle")}
)

#Define the method to calculate the MLE
setMethod(f = "mle",
          definition = function(y){
            #define n
            n = length(y)
            #code the MLE equation provided
            MLE = sum(y)/n
            return(MLE)
          }
)
