#set.seed(1)
cv_values <- NULL
log_values <- NULL
for (i in 1:1000){
  x = rnorm(n=100, mean=5, sd=1)
  cv_values[i] <- sd(x)/mean(x)
  log_values[i] <- sd(log(x))
}
plot(x=cv_values, y=log_values)
abline(lm(log_values ~ cv_values), col='red')