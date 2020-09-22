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

classif_kNN = function(orderedXl, k){
  
  classes = orderedXl[1:k, 3];
  
  counts = table(classes);
  
  class = names(which.max(counts));
  
  return (class)
}

kNN = function(xl, z, k){
  return(classif_kNN(sortObjectsByDist(xl, z), k))
}

xl = iris[,3:5];

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

x = 1;
y = 0;

while(x <= 7){
  while(y <= 3){
    class <- kNN(xl, c(x,y), k=6);
    points(x, y, pch = 22, bg = colors[class], col = colors[class], asp = 1);
    y = y + 0.5;
  }
  y = 0;
  x = x + 0.5;
}


