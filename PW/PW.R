sortObjectsByDist = function(xl, z){
  
  l = dim(xl)[1];
  n = dim(xl)[2]-1;
  
  distances = matrix(NA, l, 2);
  
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

classif_PW = function(xl, h, orderedXl, core = core_1) {
  l = dim(xl)[1];
  n = dim(xl)[2];
  
  weights = table(xl[1:l, n]);
  weights[1:dim(weights)] = 0;
  
  for (i in 1:l) {
    weights[xl[i, n]] = weights[xl[i, n]] + core(orderedXl[i] / h);
  }
  
  if (max(weights) != 0)
    return (which.max(weights))
  else
    return (0)
}

PW = function(xl, h, z){
  return(classif_PW(xl, h, sortObjectsByDist(xl, z)));
}

xl = iris[,3:5];

colors <- c("1" = "red", "2" = "green3", "3" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

x = 1;
y = 0;

while(x <= 7){
  while(y <= 3){
    class <- PW(xl, h = 1, c(x,y));
    points(x, y, pch = 22, col = colors[class], asp = 1);
    y = y + 0.5;
  }
  y = 0;
  x = x + 0.5;
}
