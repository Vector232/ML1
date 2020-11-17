sortObjectsByDist = function(xl){
  z = c(0,0)
  
  l = dim(xl)[1];
  n = dim(xl)[2]-1;
  
  distances = matrix(NA, l, 2);
  
  for (i in 1:l){
    distances[i, ] = c(sqrt(sum((xl[i, 1:n] - z)^2)), xl[i, 3]);
  }
  
  orderedXl = xl[order(distances[, 1]),];
  
  return (orderedXl);
}

r = 50
r = r * 2

xl = matrix(NA, r, 3)
xl[,1] = runif(r,0,100)
xl[,2] = runif(r,0,100)
xl[1:(r/2),3] = 1
xl[(r/2):r,3] = 2
#print(xl)
xl = iris[51:150,3:5]
class = unique(xl[,3])

sortedxll = sortObjectsByDist(xl)
#print(sortedxll)
la = nrow(xl[xl[,3]==class[1],])
lb = nrow(xl[xl[,3]==class[2],])
FRP = 0
TRP = 0
AUC = 0
x = 0
y = 0
plot(x,y, pch = 21, bg = "yellow", main = "ROC", xlim=c(0,la+1), ylim=c(0,lb+1))
colors = c("red","green","blue")
for(i in 1:nrow(xl)){
  if(sortedxll[i,3]==class[2]){
    x = x + 1
    points(x, y, pch = 21, bg = colors[sortedxll[i,3]])
    add = 1/la
    FRP = FRP + add
    AUC = AUC + TRP*add
  }
  else if(sortedxll[i,3]==class[1]){
    y = y + 1
    points(x, y, pch = 21, bg = colors[sortedxll[i,3]])
    TRP = TRP + (1/lb)
  }
}

print(FRP)
print(TRP)
print(AUC)
