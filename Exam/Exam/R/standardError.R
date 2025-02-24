#' Standard Error
#'
#' Calculates the standard error for the data, using one or two methods; standard, or bootstrap
#'
#' @param y The vector of observed data
#' @param SEtype The type of SE to be calculated; basic, or bootstrapped.
#' @param B The number of bootstraps to run, default set to 1000. Not used for basic SE estimation.
#'
#' @return A single numeric output:
#'  \item{SE}{Standard error from the type that was chosen, "basic", or "bootstrapped".}
#' @author Jordon Newton
#' @note The MLE used is calculated using the mle() function from this package. The basic SE is the square root of MLE/n. The bootstrapped version samples n observations from the dataset with replacement B times. The resulting SE is the SD of the MLEs from this matrix.
#' @examples
#'
#' set.seed(1625)
#' y <- rpois(1000,80)
#' standardError(y,"basic")
#' standardError(y,"bootstrapped",1000)
#'
#' @seealso \code{\link{estimatePois}}
#' @seealso \code{\link{logLik}}
#' @seealso \code{\link{mle}}
#' @aliases SE
#'
#' @rdname standardError
#' @export

#Set up the generic
setGeneric(name = "standardError",
           def=function(y,SEtype,B=1000)
           {standardGeneric("standardError")}
)

#Define the method to calculate the SE
setMethod(f = "standardError",
          definition = function(y,SEtype=c("basic","bootstrapped"),B){
            if(SEtype=="basic"){
              StdE <- sqrt(mle(y)/length(y))
            }
            else if(SEtype == "bootstrapped"){
              #Put a few integer checks on B
              if(B%%1!=0){stop("B must be an integer")}
              if(B<1){stop("B must be a positive number greater than 0")}
              if(B==1){stop("You can't calculate the standard deviation of a single output; pick a number of bootstraps greater than 1")}
              #Create a matrix for storing the data
              bootmat <- data.frame(nrow=length(y))
              #Run a for loop to populate B samples
              for(i in 1:B){
                bootmat <- cbind(bootmat,sample(y,length(y),replace = T))
              }
              #This method adds a junk initial row, so we'll remove thatt here
              bootmat <- bootmat[-1]
              #The one that works properly for all numbers except 1
              mlevec <- apply(bootmat, 2, mle)
              #calculate the Standard error
              StdE <- sd(mlevec)
            }
            else{stop("Please input an accurate value for SEtype: basic, bootstrapped")}
            #Return our final value
            return(StdE)
          }
)
