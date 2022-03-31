## lab 05 - part 1 ## -------------------------------------------------

myT = read.table('data/cancerRisk.txt', header = TRUE, sep='\t')

lifetime_cancer_risk = myT[ , c('Lifetime_cancer_risk')]
cum_cell_div = myT[ , c('CumulativeCellDivisions')]

plot(log10(cum_cell_div), log10(lifetime_cancer_risk))

myLm = lm(lifetime_cancer_risk~cum_cell_div)

plot(myLm)
abline(myLm, col='green')

sumT = summary(myLm)

anova( myLm)$"Pr(>F)"[1]
cor(lifetime_cancer_risk, cum_cell_div) * cor(lifetime_cancer_risk, cum_cell_div)

paste('p-value: ', sumT$coefficients[2,4])
paste('r-squared value: ',sumT$r.squared)


## lab 05 - part 2 ## -------------------------------------------------

myTcaseCont = read.table('data/caseControlData.txt', header = TRUE, sep='\t')

myTbmi = read.table('data/BMI_Data.txt', header = TRUE, sep='\t')


casecont_sample = myTcaseCont[ , c("sample")]
casecont_col = colnames(myTcaseCont)
casecont_col = casecont_col[! casecont_col %in% c('sample')]

bmi_val = myTbmi[, c("bmi")]
bmi_id = myTbmi[, c("studyid")]

case_id = vector()
for (i in 1:length(casecont_sample)) {
  subStr = substring(casecont_sample[i],1,10)
  case_id[i] = subStr
}

new_bmi_id = vector()
new_bmi_val = vector()
index = 1
for (i in 1:length(bmi_id)) {
  id = bmi_id[i]
  val = bmi_val[i]
  if (id %in% case_id == TRUE){
    new_bmi_id[index] = id
    new_bmi_val[index] = val
    index = index + 1
  }
}

new_bmi_val[29] = mean(new_bmi_val, na.rm = TRUE)

p_value = vector()

for (i in 1:length(casecont_col)) {
  col_name = casecont_col[i]
  otu = myTcaseCont[ , c(col_name)]
  myLmBMI = lm(new_bmi_val~otu)
  pVal = anova( myLmBMI)$"Pr(>F)"[1]
  p_value[i]=pVal
}

hist(p_value, breaks = 25)

pValue_adj = p.adjust(p_value, method = 'BH')

sum(pValue_adj <= 0.1)
