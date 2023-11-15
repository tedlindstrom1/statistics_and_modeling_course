library(ggplot2)

logit = function(x) log(x/(1-x))
invlogit = function(x) 1/(1+exp(-x))
x = runif(200)
logit_x = logit(x)
par(mfrow=c(2,2))
hist(x, las=1)
hist(logit_x, las=1)

xx = seq(-5, 5, 0.01)
plot(xx, invlogit(xx), type="l", las=1,
     xlab="Logit (x)",
     ylab="P")
plot(x, invlogit(logit_x), las=1)

x = rnorm(200, 10, 3)
eta = -2 + 0.4*x + rnorm(200, 0, 2)
p = invlogit(eta)
y = rbinom(200, 1, p)
par(mfrow=c(1,3))
plot(x, eta, las=1)
plot(x, p, las=1)
plot(x, y, las=1)

m = glm(y~x, family=binomial(link="logit"))
summary(m)

# Exercise 1: Add probability line to initial plot

coefs = summary(m)$coef
x_pred = seq(from=min(x), to=max(x), by=0.01)
y_hat = coefs[1,1] + coefs[2,1]*x_pred
p_hat = invlogit(y_hat)

binom_df <- data.frame(x,y) # the original data set
prob_df <- data.frame(x_pred,p_hat) # data frame for the probabilities
logit0 <- -coefs[1,1]/coefs[2,1] # calculate logit value at 0

binom_plt <- ggplot(binom_df, aes(x=x,y=y)) +
  geom_point() +
  geom_line(data=prob_df,aes(x=x_pred,y=p_hat)) +
  geom_vline(xintercept = logit0,linetype="dashed") +
  geom_hline(yintercept = 0.5,linetype="dashed")
print(binom_plt)

# end of exercise

y_hat = coefs[1,1] + coefs[2,1]*x
p_hat = invlogit(y_hat)
mean(p_hat[which(y==1)]) - mean(p_hat[which(y==0)])