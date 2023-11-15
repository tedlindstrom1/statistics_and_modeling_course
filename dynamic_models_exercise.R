# Initiate starting values:
n0 <- 1
r0 <- 1
K <- 100
h <- 0.2
r <- 0.5
# Create a list of parameters for models

P <- list(r0=r0,K=K,h=h,r=r)

# 1. plot dn/dt:
xval_dndt <- seq(1,150,1)
yval_dndt <- NULL
for (n in xval_dndt){ # you could skip this for-loop. See #3
  yval_dndt[n] <- P$r0 * n * ( 1 - n / P$K )
}
plot(x=xval_dndt, y=yval_dndt,type='l')


# 2. Plot population values
popval <- NULL
for (n in 1:20){ # could do without for-loop, see #3
  popval[n] <- P$K/(1+((K/n0)-1)*exp(-P$r0*n))
}
plot(x=1:20,y=popval,type='l')

# 3. Numerical estimate of dn/dt. 
timevector <- seq(0,20,0.1) # Create time values with step size 0.1
num_solv_n <- P$K/(1+((K/n0)-1)*exp(-P$r0*timevector)) # Generate n-values

delta_t <- diff(timevector) # difference in time steps, all vals will be 0.1
delta_n <- diff(num_solv_n) # difference between adjacent n-values
delta_nt <- delta_n/delta_t # deltan/deltat. In this case all deltan/0.1
n_plot <- num_solv_n[-length(num_solv_n)] # remove last value of num_solv_n

plot(x=n_plot,y=delta_nt,type='l') # plot numeric dn/dt against n values

# 4. Numerical solution of logistics growth function

# i) Start by setting 𝑥 = 𝑥0 , 𝑡 = 0
# ii) Choose a small Δ𝑡
# iii) Calculate 𝑓(𝑡, 𝑥)
# iv) Calculate Δ𝑥 = 𝑓(𝑡, 𝑥)Δt
# v) Update 𝑥 ← 𝑥 + Δ𝑥, 𝑡 ← 𝑡 + Δ𝑡
# vi) Repeat from iii), until reaching final t.

time_step <- seq(0,20,0.1)
y_vals <- c(1)
for (i in 2:length(time_step)){
  temp_y <- P$K/(1+((K/n0)-1)*exp(-P$r0*time_step[i]))
  dy <- diff(c(y_vals[i-1],temp_y))
  y_vals[i] <- y_vals[i-1] + dy
}

plot(x=time_step, y=y_vals, type='l',col='red')
plot(x=1:20,y=popval,type='l',col='black')

temp_eq <- function(func, x0, t0, tmax){
  x <- x0
  y_vals <- c(x)
  time_seq <- seq(t0,tmax,0.1)
  
  for (i in 2:length(time_seq)){
    
    temp_x <- func(time_seq[i],x)
    
    dx <- diff(c(y_vals[i-1],temp_x))
    
    x <- y_vals[i-1] + dx
    
    y_vals[i] <- x
  }
  return(list(y=y_vals,t=time_seq))
}

exp_growth <- function(t,x){
  grwt <- x*P$r
  return(grwt)
}
x0 <- 1
t0 <- 0
tmax <- 30
test <- temp_eq(exp_growth,x0,t0,tmax)

