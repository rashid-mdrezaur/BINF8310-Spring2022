## Lab03 ---- Problem#1, Part#B ----------------------------------------
#-----------------------------------------------------------------------
#                     The Metropolis algorithm
#-----------------------------------------------------------------------
rm(list = ls())
piOld = 0.5

nIter = 1000
postDistMetro = vector(length = nIter)

for (i in 1:nIter) {
  
  pOld = (dexp(piOld, rate=5)/0.9932621) * dbinom(14,24, piOld)
  
  piNew = piOld + rnorm(1,0,sd=0.01)
  
  if (piNew > 1) {piNew=1}
  else if (piNew <0) {piNew=0}
  
  pNew = (dexp(piNew, rate=5)/0.9932621) * dbinom(14,24, piNew)
  
  ratio = pNew/pOld
  
  if (ratio >1 || ratio >= runif(1)){
    piOld = piNew
  }
  
  postDistMetro[i] = piOld
}

myHist = hist(postDistMetro, breaks =200, plot=FALSE)
plot(myHist$mids, myHist$counts/nIter, col='darkblue')
dexpsum = sum(dexp(myHist$mids, rate=5)/0.9932621)
lines(myHist$mids, (dexp(myHist$mids, rate=5)/0.9932621)/dexpsum, col='blue')

#-----------------------------------------------------------------------
#                     Compare Metropolis with prior beta(40,40)
#-----------------------------------------------------------------------

piOld = 0.5

nIter = 1000
postDistMetro = vector(length = nIter)

for (i in 1:nIter) {
  
  pOld = dbeta(piOld, 40, 40) * dbinom(14,24, piOld)
  
  piNew = piOld + rnorm(1,0,sd=0.01)
  
  if (piNew > 1) {piNew=1}
  else if (piNew <0) {piNew=0}
  
  pNew = dbeta(piNew, 40, 40) * dbinom(14,24, piNew)
  
  ratio = pNew/pOld
  
  if (ratio >1 || ratio >= runif(1)){
    piOld = piNew
  }
  
  postDistMetro[i] = piOld
}

myHist = hist(postDistMetro, breaks =200, plot=FALSE)
par(new=TRUE)
# plot(myHist$mids, myHist$counts/nIter, col='lightgreen')
dexpsum = sum(dbeta(myHist$mids, 40+14, 40+10))
lines(myHist$mids, dbeta(myHist$mids, 40+14, 40+10)/dexpsum, col='green')


## Lab03 ---- Problem#1, Part#B ----------------------------------------
#-----------------------------------------------------------------------
#                     Grid approximation
#-----------------------------------------------------------------------
rm(list = ls())
numBreaks = 1000
postDistGrid = vector(length = numBreaks)
xVals = seq(0,1, 1/numBreaks)

i = 1
sum = 0
for (x in xVals) {
  
  postDistGrid[i] = (dexp(x, rate=5)/0.9932621) * dbinom(14, 24,x)
  sum = sum + postDistGrid[i]
  i = i+1
  
}

plot(postDistGrid / sum, col='darkorange')
lines((dexp(xVals, rate=5)/0.9932621)/sum(dexp(xVals, rate=5)/0.9932621), col='orange')

#-----------------------------------------------------------------------
#                     Compare Grid with prior beta(40,40)
#-----------------------------------------------------------------------

numBreaks = 1000
postDistGrid = vector(length = numBreaks)
xVals = seq(0,1, 1/numBreaks)

i = 1
sum = 0
for (x in xVals) {
  
  postDistGrid[i] = dbeta(x, 40, 40) * dbinom(14, 24,x)
  sum = sum + postDistGrid[i]
  i = i+1
  
}

par(new=TRUE)
plot(postDistGrid / sum, col='darkred')
lines(dbeta(xVals, 40+14, 40+10)/sum(dbeta(xVals, 40+14, 40+10)), col='red')
