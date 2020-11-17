LC = function(xl){
  return(nrow(xl))
}

risk = function(xl, classes, point, class){
  
  thisclasses = xl[which(classes == class),]
  otherclasses = xl[which(classes != class),]
  
  thisrisk = LC(thisclasses )
  otherrisk = LC(otherclasses )
  
  return(thisrisk-otherrisk)
}

SG = function(xl, nu, lyumbda){
  n = nrow(xl)
  w = runif(n,-1/(2*n),1/(2*n))
  Q = 0
  for(i in 1:n){
    Q = Q + risk(xl, unique(xl[,3]), xl[i,1:2], xl[i,3])
  }
  
  print(Q)
}


xl = iris[0:100,3:5]
SG(xl, 1, 1)

