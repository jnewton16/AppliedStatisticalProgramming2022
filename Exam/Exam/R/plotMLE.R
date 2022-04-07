#' Plot MLE
#'
#' Plots the MLE with standard errors.
#'
#' @param y The vector of observed data
#' @param SEtype The type of SE to be calculated; basic, or bootstrapped.
#' @param B The number of bootstraps to run, default set to 1000 (not required for basic SEs). Default is 1000
#' @param lambda_width The distance from the estimated MLE to be included in the plot on either side of the MLE. Default is 2.5.
#'
#' @return A plot
#'  \item{MLE Plot}{A plot that shows potential values of lambda, and the log likelihood of each, with the standard errors of MLE surrounding the maximum estimate.}
#' @author Jordon Newton
#' @note The MLE used is calculated using the mle() function from this package, the log likelihood using logLik, and the SEs using "standardError".
#' @examples
#'
#' y <- rpois(1000,80)
#' plotMLE(y,"basic")
#' plotMLE(y,"bootstrapped",100,5)
#'
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{mle}}
#' @seealso \code{\link{standardError}}
#' @aliases plotmle
#' @import ggplot2
#' @rdname plotMLE
#' @export

#set the generic for the plot
setGeneric(name = "plotMLE",
           def=function(y,SEtype,B=1000,lambda_width=2.5)
           {standardGeneric("plotMLE")}
)

#Define the method to plot
setMethod(f = "plotMLE",
          definition = function(y,SEtype=c("basic","bootstrapped"),B,lambda_width){
            lambda_hat <- mle(y)
            standard_error <- standardError(y,SEtype,B)
            lambda_vec <- seq(lambda_hat-lambda_width,lambda_hat+lambda_width,by=.01)
            ll_vec <- logLik(y,lambda_vec)
            mle_plot <- ggplot() + geom_point(aes(lambda_vec,ll_vec)) +
              labs(x = "Lambda", y = "Log Likelihood") +
              geom_vline(aes(xintercept = lambda_hat+1.96*standard_error),color="red") +
              geom_vline(aes(xintercept = lambda_hat-1.96*standard_error),color="red")
            return(mle_plot)
          }
)
