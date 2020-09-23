sortObjectsByDist = function(xl, z){
  
  l = dim(xl)[1];
  n = dim(xl)[2]-1;
  
  distances = matrix(NA, l, 2);
  
  for (i in 1:l){
    distances[i, ] = c(i, sqrt(sum((xl[i, 1:n] - z)^2)));
  }
  
  orderedXl = xl[order(distances[, 2]), ];
  return (orderedXl);
}

k1NN = function(xl, z){
  orderedXl = sortObjectsByDist(xl, z);
  
  class = orderedXl[1,3];
  
  return (c(class))
}


xl = iris[,3:5];

colors <- c("1" = "red", "2" = "green3", "3" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

x = 1;
y = 0;

while(x <= 7){
  while(y <= 3){
    class <- k1NN(xl, c(x,y));
    points(x, y, pch = 22, bg = colors[class], col = colors[class], asp = 1);
    
    y = y + 0.5;
  }
  y = 0;
  x = x + 0.5;
}
