## statistical analysis of covid data

df = read.csv('covid_dataset/us-covid19/us_total.csv', header = TRUE)

new_case = df[, c('new_cases')]

hist(new_case)

plot(density(new_case))

new_case
