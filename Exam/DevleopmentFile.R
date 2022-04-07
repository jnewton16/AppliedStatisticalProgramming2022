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
set.seed(1625)
y2 <- rpois(25,6)
mle(y)
mle(y2)
logLik(y)
logLik(y2)
#The mle estimate and logLik functions are working properly.

#test the standardError and confirm it gives correct results
standardError(y,"basic")
sqrt(mle(y)/length(y))

standardError(y2, "basic")
sqrt(mle(y2)/length(y2))
#Test the bootstrap
standardError(y,"bootstrapped",100)
standardError(y2,"bootstrapped")

#Test a few that should not work
standardError(y,"bootstrapped",.8)
standardError(y,"bootstrapped",1)
standardError(y,"booted")

#Test the final output
estimatePois(y,"basic")
estimatePois(y,"bootstrapped",1000)

estimatePois(y2,"basic")
estimatePois(y2,"bootstrapped")


#test the plot
plotMLE(y,"basic")
plotMLE(y,"bootstrapped",lambda_width = 5)

plotMLE(y2,"basic")
plotMLE(y2,"bootstrapped", lambda_width = 1)

#Test the help functions
?mle()
?logLik()
?standardError()
?estimatePois()
?PoisMLE
?plotMLE
