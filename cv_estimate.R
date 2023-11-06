set.seed(1)
x = rnorm(n=100, mean=5, sd=1)
cv <- sd(x)/mean(x)

out = NULL
for (i in 1:1000) {
  sample = sample(x, replace=TRUE)
  out[i] = sd(sample)/mean(sample)
}
hist(out, las=1, main="")
quantile(out, c(0.025, 0.975))
cat(cv)