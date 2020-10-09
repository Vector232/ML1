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
  print(k)
  classes = orderedXl[1:k, 3];
  
  counts = table(c(classes));
  
  class = names(which.max(counts));
  
  return (class)
}

kNN = function(xl, z, k){
  return(classif_kNN(sortObjectsByDist(xl, z), k))
}

LOO = function(xl, sort = sortObjectsByDist, classification = classif_kNN){
  n = nrow(xl);
  DataA = array(0,n);
  
  for(i in 1:n){
    orderedXl = sort(xl[-i,], c(xl[i,1], xl[i,2]));
    
    for(k in 1:n-1){
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

xl = matrix(0,15,3);
xl[1:5,3]  = 1
xl[6:10,3]  = 2
xl[11:15,3]  = 3

xl[1,] = c(3,2,1)
xl[2,] = c(4,1,1)
xl[3,] = c(2,4,1)
xl[4,] = c(6,2,1)
xl[5,] = c(3,1,1)
xl[6,] = c(4,8,2)
xl[7,] = c(5,9,2)
xl[8,] = c(5,7,2)
xl[9,] = c(9,8,2)
xl[10,] = c(5,11,2)
xl[11,] = c(11,13,3)
xl[12,] = c(12,6,3)
xl[13,] = c(9,5,3)
xl[14,] = c (14,10,3)
xl[15,] = c(13,12,3)

DataA = LOO(xl);  
DataA = LOO(xl);  

plot(DataA, type ="l", main="LOO(k)")

x = which.min(DataA);
y = DataA[which.min(DataA)];

label = paste("k = ", x, "\n", "LOO = ", y)
text(x-2, y+0.2, label, pos = 4);
points(x, y,pch = 22 , bg = "red")