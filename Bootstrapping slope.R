# Create data
set.seed(85)
x = rnorm(n=200, mean=10, sd=2)
y = 0.4*x + rnorm(200, 0, 1)

# Build model and extract coefficients for slope/intercept
m = lm(y~x)
cf = m$coef

# Create line to keep within data range
newx = seq(min(x), max(x), length.out=200) # New x values
predy = cf[1] + cf[2]*newx # Predicted y values for line

# Plot generated data and add regression line with lines()
plot(x, y, las=1,
     xlab="Leaf length (mm)",
     ylab="Leaf width (mm)")
lines(newx, predy)

# Exercise: Use non-parametric bootstrapping to derive a standard error for 
# the slope of the linear regression

# Setup dataframe and vector to store slope values
slopes <- NULL
df <- data.frame(x,y)

# Run bootstrapping and estimate new slopes
for (i in 1:nrow(df)){
  ndf <- df[sample(nrow(df),replace=T),] # Sample from the dataframe with replacement
  nm <- lm(m, ndf) # fit new data to the model established at the top
  nparam <- nm$coef # Extract parameters
  slopes[i] <- nparam[2] # Save slope
}
sdslopes <- sd(slopes)
sderr <- sdslopes/sqrt(nrow(df))
cat(sdslopes, "\n", sderr)