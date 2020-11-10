f = function(x, classes, aprior, mus, cov) {
  l = length(classes)
  n = ncol(mus)
  best = rep(0, l)
  
  for (i in 1:l) {
    mu = matrix(mus[i,], n, 1)
    best[i] = log(aprior[i]) - 1/2*t(mu)%*%cov%*%mu + x%*%cov%*%mu
  }
  which.max(best)
}

LDF = function(xl){
  n = ncol(xl)-1
  l = nrow(xl)
  classes = levels(xl[,n+1])
  m = length(classes)
  
  mus = matrix(0, m, n)
  covs = matrix(0, n, n)
  aprior  = rep(0, m)
  
  for (i in 1:m) {
    class = factor(classes[i], classes)
    xll = xl[xl[,n+1]==class,]
    aprior [i] = length(xll[,1]) / length(xl[,1])
    for (j in 1:n) {
      mus[i,j] = mean(xll[,j])
    }
  }
  
  for (k in 1:l) {
    for (i in 1:n) {
      for (j in 1:n) {
        c = xl[k,n+1]
        covs[i,j] = covs[i,j] + (xl[k,i] - mus[c,i]) * (xl[k,j] - mus[c,j])
      }
    }
  }
  covinv = solve(covs/l)
  
  
  classificationmap(function(x) f(x, classes, aprior , mus, covinv), xl)
}

classificationmap = function(classifier, xl) {
  
  colors=c("1"="red", "2"="green", "3"="blue", "-1"="gray")
  plot(xl[,1], xl[,2], bg=colors[xl[,3]],  col=colors[xl[,3]], pch=21, main="LDF", xlim=c(1,7),ylim=c(0,3))
  for (i in seq(1,7,0.1)) {
    mat = data.frame()
    for (j in seq(0,3,0.1)) {
      class = classifier(c(i, j))
      mat = rbind(mat,c(i, j, class))
    }
    points(mat[,1], mat[,2], col=colors[mat[,3]], pch=22)
  }
}

xl = iris[,3:5]

LDF(xl)
