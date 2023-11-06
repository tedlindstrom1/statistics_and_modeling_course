set.seed(1)
cv_values <- NULL
log_values <- NULL
sdvals <- runif(200,2,5)
for (i in 1:100){
  x = rnorm(n=100, mean=5, sd=3)
  cv_values[i] <- sd(x)/mean(x)
  log_values[i] <- sd(log(x))
}
cat('cv_values: ', cv_values)
cat('\nlog_values: ', log_values)
plot(x=cv_values, y=log_values)
abline(lm(log_values ~ cv_values), col='red')
