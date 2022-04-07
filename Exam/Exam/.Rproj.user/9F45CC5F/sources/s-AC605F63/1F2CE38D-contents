#Load libraries necessary for the exam
library(devtools)
library(roxygen2)

#Set the exam wd
setwd("C:/Users/jordo/Documents/GitHub/AppliedStatisticalProgramming2022/exam/")

#Verify the package and testing
current.code <- as.package("exam")
load_all(current.code)
document(current.code)
test(current.code)


#Test some data to run through things
y <- c(8,5,100,6,9,75,15,26,34,33,29,46,80)
mle(y)
logLik(y,mle(y))

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
