#' Estimate Poisson
#'
#' Estimates a poisson distribution given a vector of data, and returns an object of the class "PoisMLE".
#'
#' @param y The vector of observed data, either known or assumed to be from a poisson distribution
#' @param SEtype The type of SEs desired, either "basic" or "bootstrapped"
#' @param B The number of bootstraps to estimate the SE if SEtype=="bootstrapped". If basic, inclusion of this parameter is unnecessary. Default is 1000.
#'
#' @return A single object output:
#'  \item{PoisMLE}{An object of type PoisMLE, which contains the MLE, log likelihood, SE, and the type of SE used.}
#' @author Jordon Newton
#' @note Returns an object of S4 classes; utilizes the other functions within this package to create the proper data.
#' @examples
#'
#' set.seed(1625)
#' y <- rpois(1000,80)
#' estimatePois(y,"basic")
#' estimatePois(y,"bootstrapped",1000)
#'
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{mle}}
#' @seealso \code{\link{standardError}}
#' @rdname estimatePois
#' @aliases estimatePoisson
#' @export

#Set up the generic
setGeneric(name = "estimatePois",
           def=function(y,SEtype,B=1000)
           {standardGeneric("estimatePois")}
)

#The function just returns an object:
setMethod(f = "estimatePois",
          definition = function(y,SEtype=c("basic","bootstrapped"),B){
            #Use the other functions to set definitions
            .mle <- mle(y)
            .ll <- logLik(y)
            .se <- standardError(y,SEtype,B)
            #Return our PoisMLE object using the above
            return(new("PoisMLE",
                       y=y,
                       MLE = .mle,
                       LL = .ll,
                       SE = .se,
                       SEtype = SEtype))
            }
)
