library(kernlab)
library(MASS)

LOSS = function(x){
  return(log2(1 + exp(-x)))
}

sigmoid = function(x){
  return(1/(1 + exp(-x)))
} 

f = function(w, obj, class, eta){
  return(w + c(eta) * obj * class * sigmoid(c(w %*% obj) * class))
} 

margin = function(w, obj, class){
   return(w %*% obj * class) 
}

GS = function(xl, eta, lyambda){
  n = ncol(xl)
  l = nrow(xl)
  
  w = matrix(c(runif(n-1, -1/(2*(n-1)), 1/(2*(n-1)))), 1, 3)
  obj <- xl[,-n]
  classes <- xl[, n]
  Q = sum(sapply(1:l, function(i) LOSS(margin(w, obj[i,], classes[i]))))
  
  ANSQ = matrix(Q, 1, 1)
  
  c = 0
  
  while(TRUE){
    c = c + 1
    
    margins = sapply(1:l, function(i) margin(w[c,],obj[i,],classes[i]))
    err = which(margins < 0)
    
    if(length(err) == 0) break
    
    if (length(err) > 0) rand = sample(err, 1)
    else rand = sample(1:l, 1)
    
    eps = LOSS(margin(w[c,], obj[rand,], classes[rand]))
    
    eta = 1 / (obj[rand,] %*% obj[rand,])^2
    if (length(err) == 0) eta = eta / 2
    
    w = rbind(w, f(w[c,], obj[rand,], classes[rand], eta))
    
    oldQ = Q
    Q = (1 - lyambda) * Q + lyambda * eps
    ANSQ = rbind(ANSQ, Q)
    
    if (abs(oldQ - Q) / max(oldQ, Q) <= 0.0001)  break
    else if (c == 30000) break
  }
  w <- cbind(w, ANSQ)
  return(w)
}

LC = function(xl){
    w = GS(xl, 1, 1/6)
  
  n = ncol(xl)
  l = nrow(w)
  
  colors = c("red","green","blue")
  plot(xl[,1:(n-2)], type = "n", main = "LC")
  abline(w[l,3]/w[l,2],-w[l,1]/w[l,2], lwd = 3, col = "green")
  points(xl[,1:(n-2)], pch = 21, col = colors[xl[,n]+2], bg = colors[xl[,n]+2])
}

normalize = function(xl) {
  for (i in 1:(ncol(xl)-1)) 
    xl[,i] = (xl[,i] - mean(xl[,i])) / sd(xl[,i])
  return(xl)
}

addcol = function(xl){
  return(cbind(xl[,1:(ncol(xl)-1)], -1, c(xl[, ncol(xl)])))
}

n = 50

mu11 = 0
mu21 = 0

mu12 = 6
mu22 = 0

sigma11 = 0.5
sigma21 = 0.5

sigma12 = 0.5
sigma22 = 0.5

xl1 = mvrnorm(n/2, c(mu11, mu21), matrix(c(sigma11, 0, 0, sigma21), 2, 2))
xl2 = mvrnorm(n/2, c(mu12, mu22), matrix(c(sigma12, 0, 0, sigma22), 2, 2))
xl = rbind(cbind(xl1, 1), cbind(xl2, -1))

xll = normalize(xl)
xll = addcol(xll)
LC(xll)
