N = function(x, mu, cov) {
  n = length(x)
  x = as.matrix(x)
  arg = matrix(x-mu, n, 1)
  return(1/sqrt(2*pi)/det(cov)*exp(-0.5 * t(arg)%*%solve(cov)%*%arg))
}

f = function(x, classes, probs, mus, covs) {
  
  l = length(classes)
  n = dim(mus)[2]
  scores = rep(0, l)
  for (i in 1:l) {
    scores[i] = probs[i] * N(x, mus[i,], covs[[i]])
  }
  
  ans = which.max(scores)
  
  return(factor(classes[ans], classes))
}

classificationmap = function(classifier, xl) {
  
  colors=c("red", "green", "blue", "gray")
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


xl = iris[96:105,3:5]
xl[1,1] = 2.5
xl[1,2] = 0

xl[2,1] = 2.5
xl[2,2] = 1

xl[3,1] = 2.5
xl[3,2] = 2

xl[4,1] = 3
xl[4,2] = 3

xl[5,1] = 2
xl[5,2] = 1.5

xl[6,1] = 0
xl[6,2] = 0.5

xl[7,1] = 1
xl[7,2] = 1

xl[8,1] = 3.5
xl[8,2] = 1

xl[9,1] = 4
xl[9,2] = 1

xl[10,1] = 5
xl[10,2] = 1
xl = iris[,3:5]


 



n = dim(xl)[2]-1
l = dim(xl)[1]
classes = levels(xl[,n+1])


classes = levels(xl[,n+1])
l = length(classes)

mus = matrix(0, l, n)
stds = matrix(0, l, n)
probs = rep(0, l)

for (i in 1:l) {
  curr = factor(classes[i], classes)
  xll = xl[xl[,n+1]==curr,]
  probs[i] = length(xll[,1]) / length(xl[,1])
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



classificationmap(function(x) f(x, classes, probs, mus, covar), xl)

