#' Log likelihood
#'
#' Calculates the log likelihood of observed data
#'
#' @param y The vector of observed data to calculate the log likelihood from
#' @param lambda the assumed value of lambda. The default value is set at the MLE of y.
#'
#' @return A single numeric output:
#'  \item{loglikelihood}{The log likelihood for the observed data, conditioned on the given value of lamda}
#' @author Jordon Newton
#' @note This log likelihood is conditional on the value of lambda. If lambda is incorrect, then it will provide an erroneous estimate. If used in conjuction with the mle function, you will have an estimate for the actual lambda.
#' @examples
#'
#' set.seed(1625)
#' y <- rpois(1000,80)
#' lambda <- mle(y)
#' logLik(y,lambda)
#'
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{mle}}
#' @seealso \code{\link{standardError}}
#' @aliases logLikelihood
#' @rdname logLik
#' @import methods
#' @import stats
#' @export

#set the generic for the loglikelihood
setGeneric(name = "logLik",
            def=function(y,lambda=mle(y))
            {standardGeneric("logLik")}
)

#Define the method to calculate the log likelihood
setMethod(f = "logLik",
          definition = function(y,lambda){
            #set n to make the code cleaner
            n <- length(y)
            #code the formula for the log likelihood we were provided
            LL <- -1*n*lambda - sum(log(factorial(y))) + log(lambda)*sum(y)
            #Return the output
            return(LL)
          }
)
