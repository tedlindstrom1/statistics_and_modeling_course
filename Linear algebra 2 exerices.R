u <- c(3,6,7)
v <- c(12,13,14)
#1:
#a:
cat(u+v)
#b:
cat(u%*%v)
#c:
cat(sqrt(u%*%u))
#d:
cat(sqrt(v%*%v))
#e
z <- u+v
cat(sqrt(z%*%z))

#2:
b <- c(1,2,3,0,0,0,4,5,6)
dim(b) <- c(3,3)
#a:
cat(b%*%u)
#b:
cat(b%*%v)
#c:
cat(b%*%(u+v))
#d:
cat(b%*%b%*%u)

#3:
x <- c(1,1,2,0)
dim(x) <- c(2,2)
#a:
det(x)
#b:
ex <- eigen(x)
#c:
val1 <- ex$values[1] * ex$values[2]
val2 <- det(x)
cat(val1, val2)
#d:
ex$vectors
#e:
v1 <- x%*%ex$vectors[1:2,1]
v2 <- x%*%ex$vectors[1:2,2]
ev1 <- ex$values[1]%*%ex$vectors[1:2,1]
ev2 <- ex$values[2]%*%ex$vectors[1:2,2]

cat(v1, ev1,"\n")
cat(v2,ev2)

#4: 
id <- c(1,0,0,0,1,0,0,0,1)
dim(id) <- c(3,3)
m <- c(8,2,-6)
idm <- id %*% m
idm
 
#5:
eid <- eigen(id)
eid$values

#6:
m_6 <- c(1,0,0,1)
dim(m_6) <- c(2,2)
val_6 <- (0.5 * m_6) %*% c(12,8)
val_6

#7: 
em_6 <- eigen(m_6*0.5)
em_6$values

#8 & 12:
draw_a_tree <- function() {
  x <- c(0,0,0.7,1.5,NA,0.7,0.8,NA,0,-0.6,-1.2,NA,-0.6,-0.5)
  y <- c(0,1,1.3,1.4,NA,1.3,1.8,NA,1,1.4,1.7,NA,1.4,2)
  plot(x,y,type='l',col='brown',lwd='8', xlim=c(-2,2),ylim=c(-2,2))
}
draw_a_tree()

#9:
draw_a_small_tree <- function() {
  x <- 0.5*c(0,0,0.7,1.5,NA,0.7,0.8,NA,0,-0.6,-1.2,NA,-0.6,-0.5)
  y <- 0.5*c(0,1,1.3,1.4,NA,1.3,1.8,NA,1,1.4,1.7,NA,1.4,2)
  
  lines(x,y,type='l',col='green',lwd='8')
}
draw_a_small_tree()

#13:
draw_a_transformed_tree <- function(A){
  x <- c(0,0,0.7,1.5,NA,0.7,0.8,NA,0,-0.6,-1.2,NA,-0.6,-0.5)
  y <- c(0,1,1.3,1.4,NA,1.3,1.8,NA,1,1.4,1.7,NA,1.4,2)
  x_trans <- NULL
  y_trans <- NULL
  for (i in 1:length(x)){
    u <- c(x[i],y[i])
    v <- A %*% u
    x_trans[i] <- v[1]
    y_trans[i] <- v[2]
  
  }
  lines(x_trans,y_trans,type='l',col='green',lwd='8')
}

A <- c(0,1,1,0)
dim(A) <- c(2,2)
draw_a_tree()
draw_a_transformed_tree(A)

#15:
theta <- -30/360*2*pi
A <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),2,2)
draw_a_tree()
draw_a_transformed_tree(A)

ei_15 <- eigen(A)
ei_15$values

#16:
A <- c(-1,0,0,1)
dim(A) <- c(2,2)
draw_a_tree()
draw_a_transformed_tree(A)