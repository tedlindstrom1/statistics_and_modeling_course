# Create parameters in a array
param <- c(0.5, -0.2, 0.3, 1.2)
dim(param) <- c(2,2)

# Create initial values of Owls and Rats
o <- 2000
r <- 1000
o_pred <- c(o)
r_pred <- c(r)

# Run through simulation. For every iteration, the model is created anew with previous values
# of o and r, then multiply the two arrays and extract the new owl and rat values. Update o and r
# Save values in two vectors
for (i in 2:10){
  model <- c(o,r)
  dim(model) <- c(2,1)
  t <- param %*% model
  o <- t[1,1]
  o_pred[i] <- o
  
  r <- t[2,1]
  r_pred[i] <- r
  cat("Owls: ",o,"Rats: ",r,"\n")
}

# Plot the two series and add legend
plot(x=1:length(o_pred), y=o_pred,col="red", xlab="Generations", ylab="Population size")
points(x=1:length(r_pred), y=r_pred, col="blue")
legend(x=8, y=1500, legend=c("Owls", "Rats"), col=c("red","blue"), lty=1:2)

