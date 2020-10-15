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

core_4 = function(r)
{
  return (((2*pi)^(-1/2)) * exp(-1/2*r^2)) 
}

classif_PF = function(xl, pots, h, orderedXl, core = core_4) {
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

 pots = c( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,
           0, 0)




colors <- c("1" = "red1", "2" = "green1", "3" = "blue1",
            "11" = "red2", "22" = "green2", "33" = "blue2",
            "111" = "red3", "222" = "green3", "333" = "blue3")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, main = "Ядро Епанечникова")
xl = iris[,3:5]

h = 1
x = 1;
y = 0;

while(x <= 7){
  while(y <= 3){
    class <- PF(xl, pots, h = 1, c(x,y));
    points(x, y, pch = 22, col = colors[class], asp = 1);
    y = y + 0.1;
  }
  y = 0;
  x = x + 0.1;
} 
