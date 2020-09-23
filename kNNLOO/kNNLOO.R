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
  
  counts = table(c(classes));
  
  class = names(which.max(counts));
  
  return (class)
}

kNN = function(xl, z, k){
  return(classif_kNN(sortObjectsByDist(xl, z), k))
}

LOO = function(xl, sort = sortObjectsByDist, classification = classif_kNN){
  n = dim(xl)[1];
  DataA = array(0,n);
  
  for(i in 1:n){
    orderedXl = sort(xl[-i,], c(xl[i,1], xl[i,2]));
    
    for(k in 1:n){
      class = classification(orderedXl, k);
      #print(class);
      #print(xl[i,3]);
      if(class != c(xl[i,3])){
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
