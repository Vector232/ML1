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

kNN = function(xl, z, k){
  orderedXl = sortObjectsByDist(xl, z);
    
  classes = orderedXl[1:k, 3];
      
  counts = table(classes);
      
  class = names(which.max(counts));

  return (class)
}


xl = iris[,3:5];

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

for(i in 1:25){
  x = runif(1, 0, 7);
  y = runif(1, 0, 2.5);
  z <- c(x, y);
  class <- kNN(xl, z, k=6);
  points(z[1], z[2], pch = 22, bg = colors[class], asp = 1);
}



