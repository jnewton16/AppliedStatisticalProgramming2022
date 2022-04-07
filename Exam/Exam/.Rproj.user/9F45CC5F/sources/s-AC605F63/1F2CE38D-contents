#Load libraries necessary for the exam
library(devtools)
library(roxygen2)

#Set the exam wd
setwd("C:/Users/jordo/Documents/GitHub/AppliedStatisticalProgramming2022/exam/")

#Verify the package and testing
current.code <- as.package("exam")
load_all(current.code)
document(current.code)
check(current.code)

#Test some data to run through things
set.seed(1625)
y <- rpois(1000,80)
mle(y)
logLik(y,mle(y))
#The mle estimate and logLik functions are working properly.

#test the standardError and confirm it gives correct results
standardError(y,"basic")
sqrt(mle(y)/length(y))
#Test the bootstrap
standardError(y,"bootstrapped",100)
standardError(y,"bootstrapped",.8)
standardError(y,"bootstrapped",1)


#Test the final output
estimatePois(y,"basic")
estimatePois(y,"bootstrapped",1000)

#Test the help functions
?mle()
?logLik()
?standardError()
?estimatePois()
?PoisMLE

