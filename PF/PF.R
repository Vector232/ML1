sortObjectsByDist = function(xl, z){
  
  l = nrow(xl);
  n = ncol(xl) - 1;
  
  distances = array(0, l);
  
  for (i in 1:l){
    distances[i] = sqrt(sum((xl[i, 1:n] - z)^2));
  }
  
  return (distances);
}

core_1 = function(r){
  
  if(r <= 1){
    return((3/4)*(1-r^2))
  }
  else
    return(0)
}

core_2 = function(r){
  
  if(r <= 1){
    return((15/16)*(1-r^2)^2)
  }
  else
    return(0)
}

core_3 = function(r){
  
  if(r <= 1){
    return((1-abs(r)))
  }
  else
    return(0)
}

classif_PF = function(xl, pots, h, orderedXl, core = core_1) {
  l = nrow(xl);
  n = ncol(xl);
  
  weights = table(xl[, n]);
  weights[1:dim(weights)] = 0;
  
  for (i in 1:l) {
    weights[xl[i, n]] = weights[xl[i, n]] + (pots[i] * core(orderedXl[i] / h));
  }
  if (max(weights) != 0)
    return (which.max(weights))
  else
    return (0)
}

PF = function(xl, pots, h, z){
  return(classif_PF(xl, pots, h, sortObjectsByDist(xl, z)));
}

SetPots = function(xl, err){
  l = nrow(xl)
  n = ncol(xl)
  error = err + 1
  pots = array(0,l)
  distances = matrix(-1,l,l)
  
  for(i in 1:l){
    distances[i,] = sortObjectsByDist(xl, c(xl[i,1],xl[i,2]))
    print(i)
  }
  
  while(error > err){
    while(TRUE){
      i = sample(1:l, 1);
      
      class = classif_PF(xl, pots, h=1, distances[i,]);
      
      if(class != c(xl[i,n])){
        pots[i] = pots[i] + 1;
        break;
      }
      error = 0;
      for(j in 1:l){
        class = classif_PF(xl[-j,], pots, h=1, distances[j,]);
        if(class != c(xl[j,n])){
          error = error + 1;
        }
      }
    }
    e = paste("error = ", error);
    print(e)
    print(pots)
  }
  print("---------------------------")
  print(pots)
  return(pots)
}




colors <- c("1" = "red", "2" = "green3", "3" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
xl = iris[,3:5]
h = 1
pots = SetPots(xl, 5)
x = 4
y = 1
class = PF(xl,pots, h, c(x,y))
points(x, y, pch = 22, col = colors[class], asp = 1)
  }
  
  return(pots)
}




colors <- c("1" = "red", "2" = "green3", "3" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
xl = iris[,3:5]
h = 0.4
pots = SetPots(xl, 5)
x = 4
y = 1
class = PF(xl,pots, h, c(x,y))
points(x, y, pch = 22, col = colors[class], asp = 1)
