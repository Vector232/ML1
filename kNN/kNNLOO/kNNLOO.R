sortObjectsByDist = function(xl, k){
  
  l = dim(xl)[1];
  z = xl[k, 1:2];
  
  distances = matrix(NA, l-1, 2);
  
  c = 1;
  
  for (i in 1:l){
     if(i != k ){
       distances[c, ] = c(i, sqrt(sum((xl[i, 1:2] - z)^2)));
       c = c + 1;
     }
  }
  
  orderedXl = xl[order(distances[, 2]), ];
  return (orderedXl);
}

LOO = function(xl){
  n = dim(xl)[1];
  DataA = array(0,n);
  
  for(i in 1:n){
    
    orderedXl = sortObjectsByDist(xl, i);
    
    for(k in 1:n){
      classes = orderedXl[1:k, 3];

      counts = table(classes);
      
      class = names(which.max(counts));
      
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

plot(DataA)
 
