#set.seed(1)
x = rnorm(n=1000, mean=5, sd=2)
y = 0.5*x + rnorm(1000, 0, 1)
xbias <- NULL
for (i in 1:length(x)){
  xbias[i] <- x[i]+runif(1,min=1, max=2)
}

ybias <- 0.5*xbias + rnorm(1000, 0, 1)

df <- data.frame(x,y,xbias,ybias)
md1 <- lm(df$y ~ df$x)
md2 <- lm(df$ybias ~ df$xbias)
par(pty="s", mfrow=c(1,2))
plot(df$x,df$y)
plot(df$xbias,df$ybias)

cf1 <- md1$coef
cf2 <- md2$coef
cat("Unbiased: ", cf1, " Biased : ", cf2)
