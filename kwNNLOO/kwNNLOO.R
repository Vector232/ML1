sortObjectsByDist = function(xl, k){
  
  l = dim(xl)[1];
  z = xl[k, 1:2];
  
  distances = matrix(NA, l-1, 2);
  
  c = 1;
  
  for (i in 1:l){
    if(i != k ){
      distances[c, ] = c(sqrt(sum((xl[i, 1:2] - z)^2)), xl[i, 3]);
      c = c + 1;
    }
  }
  
  orderedXl = distances[order(distances[, 1]),];
  
  return (orderedXl);
}

LOO = function(xl){
  n = dim(xl)[1];
  DataA = array(0,n);
  
  for(i in 1:n){
    
    orderedXl = sortObjectsByDist(xl, i);
    
    
    for(k in 1:(n-1)){
      weightClasses = table(xl[0, 3]);
      
      for(j in 1:k){
        if(orderedXl[j,2] == 1){ weightClasses[1] = weightClasses[1] + (1/(0.1+ orderedXl[j,1]));}
        else if(orderedXl[j,2] == 2){ weightClasses[2] = weightClasses[2] + (1/(0.1+ orderedXl[j,1]));}
        else if(orderedXl[j,2] == 3){ weightClasses[3] = weightClasses[3] + (1/(0.1+ orderedXl[j,1]));}
      }
      
      class = names(which.max(weightClasses));
      
      if(class != xl[i,3]){
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
print(which.min(DataA));

plot(DataA)