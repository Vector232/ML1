frasp = function(z, mu, m){
  
  mu = as.numeric(mu)
  nm = as.matrix(z-mu)
    
  tnm = t(nm)
  sm = solve(m)
  
  a = exp( (nm %*% sm %*% tnm) / (-2) )
 
  b = sqrt( ( (2*pi)^nrow(m) )*det(m) )
    
  ans = a / b
  
  return(ans)
  }

fnewmatrix = function(xl, mu){
  n = nrow(xl)
  l = ncol(xl)
  
  
  mu = t(as.matrix(as.numeric(mu[1 , 1:l - 1])))
  
  ans = matrix(0, l-1, l-1)
  
  for(i in 1:n){
    a = as.matrix(xl[i, 1:l-1], 1)
    
    m = t(a - mu) %*% (a - mu)
    ans = ans + m
  }
  ans = ans / (n - 1)
  
  return(ans)
}

fmu = function(xl){
  l = ncol(xl)
  ans = data.frame()
  
  for(i in unique(xl[,l])){
    signs = xl[xl[,l] == i, ]
    m = nrow(signs)
    
    ismu = vector()
    
    for(j in 1:(l-1)){
      ismu = cbind(ismu, sum(signs[,j])/m)
    }
    
    ans = rbind(ans, c(ismu, i))
  }
  return(ans)
}

faprior = function(xl){ # априорные вероятности
  n = nrow(xl)
  l = ncol(xl)
  
  ans = data.frame(table(xl[,l]) / n)
  
  return(ans)
}

PI = function(z, xl){
  mu = fmu(xl)
  aprior = faprior(xl)
  l = dim(mu)[1]
  
  lyambda = c(1, 1, 1)
  max = -1
  best = aprior[1,1]
  
  for(i in 1:l){
    
    class = aprior[i,1]
    
    x = xl[c(xl[,3])==i,]
    M = fnewmatrix(x , mu[i,])
   
    tight = frasp(z, mu[i, 1:l-1], M)
    
    lr = lyambda[i] * aprior[i, 2] * tight 
    
    if(lr > max){
      max = lr
      best = class
    }
  }
  if(max < (-0.9999)){
    best = 4
  }
  
  ans = c(best, max)
  
  return(ans)
}
