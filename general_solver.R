# Write a general ode-solver, taking the function f(t,x) as an input parameter. 
rm(list=ls())
my_ode_solver <- function(f, x0, t0, t_max) {
  delta_t <- 0.1 # fixed time-step

  # vector of t-values:
  t <- seq(t0, t_max, by=delta_t)
  
  # vector of calculated solution:
  x_solution <- rep(0, length(t))
  
  #  Start by setting x=x0
  x <- x0
  x_solution[1] <- x0
  
  for (ti in 2:length(t)) {
    # Calculate dx/dt = f(t,x)
    dxdt <- f(t[ti-1], x) # We use derivative at t-delta_t to calculate x at t
    
    # Calculate Δx=f(t,x)Δt
    delta_x <- dxdt*delta_t
    
    # Update x
    x <- x + delta_x
    
    # Store solution:
    x_solution[ti] <- x
  }
  
  # return a list with t and x-values:
  return(list(t=t, x=x_solution))
}

# Test with exponential growth:
r <- 0.5 # exponential growth rate
exp_growth <- function(t,x) {
  dxdt <- r*x
  return(dxdt)
}

x0 <- 1
t0 <- 0
t_max <- 20
solution <- my_ode_solver(exp_growth, x0, t0, t_max)
plot(solution$t, solution$x, type='l', col='blue', xlab='t', ylab='x(t)', main='Exponential growth')
# compare to exact solution:
x_exact <- x0*exp(r*solution$t)
lines(solution$t, x_exact, col='black', lty='dotdash' )
legend('topleft', legend=c('numerical solution','exact solution'), col=c('blue','black'), lty=c('solid','dotdash'))