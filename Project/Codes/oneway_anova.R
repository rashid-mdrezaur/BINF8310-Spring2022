myT = read.csv('covid_dataset/state_data/new_state_data.csv', header = TRUE, check.names = FALSE)

myT


myData_confirm = as.numeric(myT[,2])
myData_death = as.numeric(myT[,3])
myData_recover = as.numeric(myT[,4])
myData_mortality = as.numeric(myT[,7])

states = myT[,1]
states = factor(states)

myLm_c = lm(myData_confirm ~ states, x=TRUE)
pvalue_confirm = anova(myLm_c)$"Pr(>F)"[1]

myLm_d = lm(myData_death ~ states, x=TRUE)
pvalue_death = anova(myLm_d)$"Pr(>F)"[1]

myLm_r = lm(myData_recover ~ states, x=TRUE)
pvalue_recovered = anova(myLm_r)$"Pr(>F)"[1]

myLm_m = lm(myData_mortality ~ states, x=TRUE)
pvalue_mortality = anova(myLm_m)$"Pr(>F)"[1]

anova(myLm_c)

anova(myLm_d)

anova(myLm_r)

anova(myLm_m)

summary(myLm_c)
