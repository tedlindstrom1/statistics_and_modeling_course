# Make a numerical test of the solution to the logistic growth equation. 
# In other words, calculate Δn/Δt for different points on the curve in exercise 2. 
# Make a suitable plot to compare the result to the correct values given by the model
# ( dn/dt=r_0*n(1-n/K) ). 

# Key parameters:
r0 <- 1 # intrinsic growth rate
K <- 100 # carrying capacity
n0 <- 1 # starting population size for numeric solution
delta_t <- 0.5 # Fixed time-step

# A vector of t-values with the given step-size
t <- seq(0, 20, by=delta_t)

# The corresponding solution n(t):
n_solution <- K/(1+(K/n0-1)*exp(-r0*t))

# A numerical approximation of dn/dt:
dndt_num <- diff(n_solution)/diff(t) # equivalent: diff(n_solution)/delta_t
# Note: The length of dndt_num is one less than the length of n_solution

# Plot dn/dt against n:
n_plot <- n_solution[-length(n_solution)] # skip last point
plot(n_plot, dndt_num, type='l', col='red', xlab='n', ylab='dn/dt')

# Compare numerical derivative with the model:
dndt_model <- r0*n_plot*(1-n_plot/K)
lines(n_plot, dndt_model, col='black', lty='dashed')

legend('topleft', legend=c('numerical derivative','exact values'), col=c('red','black'), lty=c('solid','dashed'))
