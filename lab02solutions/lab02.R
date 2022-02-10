
## LAB02 - Problem#1 - Part 1 -----------------------------------------
#----------------------------------------------------------------------

data = c(2,3,2,6,3,5,6,2,6,6,2,6,6,2,3,6,6,6,5,6,6,5,6,6,6,6,6,4,6,3,3,3,6,6,5,6,6)

dieLoadPrior = c(0.01, 0.99)

dieLoadLL = c(.1,.1,.1,.1,.1,.5)
dieFairLL = c(1/6,1/6,1/6,1/6,1/6,1/6)

dieLoadPost = vector()

titleStr = ""
for (i in 1:length(data)) {
  dieLoadPost[i] = dieLoadPrior[1]
  
  dnorm = dieLoadPrior[1]*dieLoadLL[data[i]] + dieLoadPrior[2]*dieFairLL[data[i]]
  
  dieLoadPrior[1] = (dieLoadPrior[1]*dieLoadLL[data[i]])/dnorm
  dieLoadPrior[2] = (dieLoadPrior[2]*dieFairLL[data[i]])/dnorm
  
  titleStr = paste(titleStr,data[i], sep="")
  plot(1:i,dieLoadPost, main = titleStr, ylim = c(0,1), xlim = c(1, length(data)+1))
  Sys.sleep(.1)
}



## LAB02 - Problem#1 - Part 2 -----------------------------------------
#----------------------------------------------------------------------

count = vector()

for (i in 1:50) {
  dieLoadPrior = c(0.01, 0.99)
  
  dieLoadLL = c(.1,.1,.1,.1,.1,.5)
  dieFairLL = c(1/6,1/6,1/6,1/6,1/6,1/6)
  
  ct = 0
  while (TRUE) {
    
    roll = sample(1:6,1,prob= c(.1,.1,.1,.1,.1,.5))
    
    dieLoadPost = dieLoadPrior[1]
    
    dnorm = dieLoadPrior[1]*dieLoadLL[roll] + dieLoadPrior[2]*dieFairLL[roll]
    
    dieLoadPrior[1] = (dieLoadPrior[1]*dieLoadLL[roll])/dnorm
    dieLoadPrior[2] = (dieLoadPrior[2]*dieFairLL[roll])/dnorm
    ct = ct + 1
    if (dieLoadPost >= 0.99999){
      count[i] = ct
      break
    }
  }
}

avgTime = ceiling(mean(count))
print(paste('On avg we have to roll the loaded die',avgTime,'times'))


## LAB02 - Problem#2 - Part 1 -----------------------------------------
#----------------------------------------------------------------------

count = vector()

for (i in 1:50) {
  ptPosPrior = c(0.001, 0.999)
  
  posPtPosLL = c(.91,0.09)
  posPtNegLL = c(0.16,0.84)
  
  ct = 0
  while (TRUE) {
    
    roll = sample(1:2,1,prob= c(.91,0.09))
    ptPosPost = ptPosPrior[1]
    
    dnorm = ptPosPrior[1]*posPtPosLL[roll] + ptPosPrior[2]*posPtNegLL[roll]
    
    ptPosPrior[1] = (ptPosPrior[1]*posPtPosLL[roll])/dnorm
    ptPosPrior[2] = (ptPosPrior[2]*posPtNegLL[roll])/dnorm
    ct = ct + 1
    if (ptPosPost >= 0.99999){
      count[i] = ct
      break
    }
  }
}

avgTime = ceiling(mean(count))
cat('For a patient with the disease,\non avg the test needs to be repeated',avgTime,'times')


## LAB02 - Problem#2 - Part 2 -----------------------------------------
#----------------------------------------------------------------------

count = vector()

for (i in 1:50) {
  ptPosPrior = c(0.001, 0.999)
  
  posPtPosLL = c(.91,0.09)
  posPtNegLL = c(0.16,0.84)
  
  ct = 0
  while (TRUE) {
    
    roll = sample(1:2,1,prob= c(0.16,0.84))
    ptPosPost = ptPosPrior[2]
    
    dnorm = ptPosPrior[1]*posPtPosLL[roll] + ptPosPrior[2]*posPtNegLL[roll]
    
    ptPosPrior[1] = (ptPosPrior[1]*posPtPosLL[roll])/dnorm
    ptPosPrior[2] = (ptPosPrior[2]*posPtNegLL[roll])/dnorm
    ct = ct + 1
    if (ptPosPost >= 0.99999){
      count[i] = ct
      break
    }
  }
}

avgTime = ceiling(mean(count))
cat('For a patient without the disease,\non avg the test needs to be repeated',avgTime,'times')
