sortObjectsByDist = function(xl, z){
  
  l = dim(xl)[1];
  n = dim(xl)[2]-1;
  
  distances = array(NA, l);
  
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

core_4 = function(r)
{
  return (((2*pi)^(-1/2)) * exp(-1/2*r^2)) 
}

classif_PW = function(xl, h, orderedXl, core = core_1) {
  l = dim(orderedXl)[1];
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

LOO = function(xl, sort = sortObjectsByDist, classification = classif_PW){
  n = dim(xl)[1]-1;
  DataA = array(0,20);
  # print(n)
  for(i in 1:n){
    orderedXl = sort(xl[-i,], c(xl[i,1], xl[i,2]));
    
    for(h in 1:20){
      class = classification(xl, h/10 ,orderedXl);
      if(class != c(xl[i,3])){
        
        #print(c(xl[i,3]))
        DataA[h] = DataA[h] + 1;
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
plot(DataA, type ="l", main = "LOO(h)", xlab = "h", ylab = "LOO")

x = which.min(DataA);
y = DataA[which.min(DataA)];

label = paste("h = ", x/10, "\n", "LOO = ", y)
text(x, y+0.2, label, pos = 4);
points(x, y,pch = 22 , bg = "red")
