#' Estimate Poisson
#'
#' Estimates a poisson distribution given a vector of data, and returns an object of the class "PoisMLE".
#'
#' @param y The vector of observed data
#' @param SEtype The type of SEs desired, either "basic" or "bootstrapped"
#' @param B The number of bootstraps to estimate the SE if SEtype=="bootstrapped". If basic, this is unnecessary.
#'
#' @return A single numeric output:
#'  \item{PoisMLE}{An object of type PoisMLE, which contains the MLE, log likelihood, SE, and the type of SE used}
#' @author Jordon Newton
#' @note Returns an object of S4 classes; utilizes the other functions within this package to create the proper data.
#' @examples
#'
#'
#'
#' @rdname estimatePois
#' @export

#Set up the generic
setGeneric(name = "estimatePois",
           def=function(y,SEtype,B)
           {standardGeneric("estimatePois")}
)

setMethod(f = "estimatePois",
          definition = function(y,SEtype=c("basic","bootstrapped"),B){
            .mle <- mle(y)
            .ll <- logLik(y,.mle)
            .se <- standardError(y,SEtype,B)
            return(new("PoisMLE",
                       y=y,
                       MLE = .mle,
                       LL = .ll,
                       SE = .se,
                       SEtype = SEtype))
            }
)
