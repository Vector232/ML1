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
  
  weightClasses = table(orderedXl[,2]);
  weightClasses[1:dim(weightClasses)] = 0
  
  for(j in 1:k){
    weightClasses[orderedXl[j,2]] = weightClasses[orderedXl[j,2]] + (orderedXl[j,1] * q^j);
    
  }
  
  class = names(which.max(weightClasses));
  
  return (class)
}

kwNN = function(xl, z, k, q){
  return(classif_kwNN(sortObjectsByDist(xl, z), k, q));
}

LOO = function(xl, sort = sortObjectsByDist, classification = classif_kwNN){
  q = 0.2;
  n = dim(xl)[1]-1;
  DataA = array(0,n);
  # print(n)
  for(i in 1:n){
    orderedXl = sort(xl[-i,], c(xl[i,1], xl[i,2]));
    
    for(k in 1:n){
      class = classification(orderedXl, k, q);
      if(class != c(xl[i,3])){
        
        #print(c(xl[i,3]))
        DataA[k] = DataA[k] + 1;
      }
    }
    print(i);
    print(DataA);
  }
  return (DataA)
}

xl = iris[,3:5];

DataA = LOO(xl);  
print(DataA);
plot(DataA, type ="l", main = "LOO(k, q = 0.1)", xlab = "k", ylab = "LOO")

x = which.min(DataA);
y = DataA[which.min(DataA)];

label = paste("k = ", x, "\n", "LOO = ", y)
text(x, y+5, label, pos = 4);
points(x, y,pch = 22 , bg = "red")
