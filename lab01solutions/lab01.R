loadedDie = c(1,2,3,4,5,6)
dieProb = c(.1,.1,.1,.1,.1,.5)

#
#calculate mean
calMean = function(die, prb){
  sumVal = 0
  len = length(die)
  for (i in 1:len) {
    sumVal = sumVal + (die[i]*prb[i])
  }
  
  return(sumVal)
}

trueMean = calMean(loadedDie, dieProb)

#
#calculate variance
calVariance = function(die, prb, mn){
  sumVal = 0
  for (i in 1:length(die)){
    sumVal = sumVal + (prb[i]*(die[i]-mn)^2)
  }
  return(sumVal)
}

trueVariance = calVariance(loadedDie,dieProb,trueMean)

#
#function to roll the die a large number of times and create a histogram
#to check if that approximate a uniform distribution

rollLoadedDie <- function(number){
  dieSamp = sample(c(1,2,3,4,5,6), 
                   size = number, 
                   replace = TRUE, 
                   prob = c(.1,.1,.1,.1,.1,.5))
  
  return(dieSamp)
}

myRolls <- rollLoadedDie(100000)

hist(myRolls, breaks = 10)

#
#try out to see number of rolls for expected mean and variance for the loaded die

trialSizes <- c(5,10,15,20,25,30,40,50,60,70,80,100,150,200,300,500,1000,
                2000,3000,4000,5000,10000,20000,50000,100000,200000,500000)
means <- vector(mode = 'double', length = length(trialSizes))
variances <- vector(mode = 'double', length = length(trialSizes))

for (i in 1:length(trialSizes)) {
  rolls <- rollLoadedDie(trialSizes[i])

  means[i] <- mean(rolls)
  variances[i] <- var(rolls)
}

plot(log10(trialSizes),means)
lines(log10(trialSizes), rep(trueMean, length(trialSizes)))

windows()

plot(log10(trialSizes),variances)
lines(log10(trialSizes), rep(trueVariance, length(trialSizes)))


