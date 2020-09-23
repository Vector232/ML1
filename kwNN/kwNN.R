sortObjectsByDist = function(xl, z){
  
  l = dim(xl)[1];
  n = dim(xl)[2]-1;
  
  distances = matrix(NA, l, 2);
  
  for (i in 1:l){
    distances[i, ] = c(sqrt(sum((xl[i, 1:n] - z)^2)), xl[i, 3]);
  }
  
  orderedXl = distances[order(distances[, 1]),];
  
  return (orderedXl);
}

classif_kwNN = function(orderedXl, k, q){

  weightClasses = table(orderedXl[,2])-50;
  
  for(j in 1:k){
     weightClasses[orderedXl[j,2]] = weightClasses[orderedXl[j,2]] + (orderedXl[j,1] * q^j);
    }
  
  class = names(which.max(weightClasses));
  print(class);
  return (class)
}

kwNN = function(xl, z, k, q){
  return(classif_kwNN(sortObjectsByDist(xl, z), k, q));
}

xl = iris[,3:5];

colors <- c("1" = "red", "2" = "green3", "3" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

x = 1;
y = 0;

while(x <= 7){
  while(y <= 3){
    class <- kwNN(xl, c(x,y), k=6, q=0.2);
    points(x, y, pch = 22, bg = colors[class], col = colors[class], asp = 1);
    y = y + 0.5;
  }
  y = 0;
  x = x + 0.5;
}