# Import bird data into dataframe

birds = read.csv("bird_allometry.csv")
brain_log <- log(birds$brain_mass)

# Log transform the data
body_log <- log(birds$body_mass)
birds_log <- data.frame(brain_log,body_log)

# Build a model for linear regression
model <- lm(birds_log$brain_log ~ birds_log$body_log)
cf <- model$coef

# Use trick in example to get a regression line within data set limits
newx <- seq(max(birds_log$body_log), min(birds_log$body_log), length.out=nrow(birds_log))
predy <- cf[1] + cf[2]*newx

# Plot model and line
plot(x=birds_log$body_log, y=birds_log$brain_log)
lines(x=newx, y=predy, col='red')
par(pty="s")