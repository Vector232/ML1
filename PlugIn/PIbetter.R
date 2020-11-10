fprob = function(x, mu, cov) {
  n = length(x)
  x = as.matrix(x)
  arg = matrix(x-mu, n, 1)
  return(1/sqrt(2*pi)/det(cov)*exp(-0.5 * t(arg)%*%solve(cov)%*%arg))
}

f = function(x, classes, aprior, mus, covs, lyambda) {
  
  l = length(classes)
  n = ncol(mus)
  scores = rep(0, l)
  for (i in 1:l) {
    scores[i] = lyambda[i] * aprior[i] * fprob(x, mus[i,], covs[[i]])
  }
  
  ans = which.max(scores)
  return(factor(classes[ans], classes))
}

PI = function(xl){
  n = ncol(xl)-1
  l = nrow(xl)
  classes = levels(xl[,n+1])
  
  
  classes = levels(xl[,n+1])
  l = length(classes)
  
  mus = matrix(0, l, n)
  stds = matrix(0, l, n)
  aprior = rep(0, l)
  lyambda = rep(1, l)
  for (i in 1:l) {
    c = factor(classes[i], classes)
    
    xll = xl[xl[,n+1]==c,]
    aprior[i] = length(xll[,1]) / length(xl[,1])
    for (j in 1:n) {
      mus[i,j] = mean(xll[,j])
      stds[i,j] = sqrt(var(xll[,j]))
    }
  }
  
  covar = list()
  for (c in 1:length(classes)) {
    xll = xl[xl[,n+1]==classes[c],1:n]
    covar[[c]] = cov(xll)
  }
  
  
  
  classificationmap(function(x) f(x, classes, aprior, mus, covar, lyambda), xl)
}

classificationmap = function(classifier, xl) {
  
  colors=c("1"="red","2"="green","3"="blue","-1"="gray")
  plot(xl[,1], xl[,2], bg=colors[xl[,3]],  col=colors[xl[,3]], pch=21, main="PlugIn", xlim=c(1,7),ylim=c(0,3))
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

PI(xl)
