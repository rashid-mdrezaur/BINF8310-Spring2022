rm(list = ls())

## Lab03 ---- Problem#1, Part#A ------------------------------------------------
#-----------------------------------------------------------------------

x = seq(0,1, 0.0001)

prior_phead = vector()

for (i in 1:length(x)) {
  if (x[i]>=0 && x[i]<=1){
    prior_phead[i] = dexp(x[i], rate = 5)/0.9932621
  }
  else{
    prior_phead[i] = 0
  }
}

plot(x, prior_phead)