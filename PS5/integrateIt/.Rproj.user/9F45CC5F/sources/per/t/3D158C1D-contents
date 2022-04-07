## Load libraries
library(devtools)
library(roxygen2)


#set wd
setwd("C:/Users/jordo/Documents/GitHub/AppliedStatisticalProgramming2022/PS5/")


## Verify the package and update documentation
current.code <- as.package("integrateIt")
check(current.code)
load_all(current.code)
document(current.code)

## Test the function
#Set up what we're passing to the function
test.f <- function(x) {return(x^2-5)
}
n <- 10
ab <- c(0,10)

#Test the first
test.trap <- integrateIt(n,test.f,ab,"Trapezoid")
#See the results
test.trap
#Check if the print method is working
print(test.trap$Integral_object)

#Test the second
test.simpson <- integrateIt(n,test.f,ab,"Simpson")
#See the results
test.simpson
#Check the print method
print(test.simpson$Integral_object)

