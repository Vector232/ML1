# Метод потенциальных функций

Метод получается, если поместить центр парзеновского окна не в классифицируемый объект, а в каждый объект обучающей выборки.

---

Код:

```R
sortObjectsByDist = function(xl, z){
  
  l = nrow(xl);
  n = ncol(xl) - 1;
  
  distances = array(0, l);
  
  for (i in 1:l){
    distances[i] = sqrt(sum((xl[i, 1:n] - z)^2));
  }
  
  return (distances);
}

classif_PF = function(xl, pots, h, orderedXl, core = core_1) {
  l = nrow(xl);
  n = ncol(xl);
  
  weights = table(xl[, n]);
  weights[1:dim(weights)] = 0;
  
  for (i in 1:l) {
    weights[xl[i, n]] = weights[xl[i, n]] + (pots[i] * core(orderedXl[i] / h));
  }
  if (max(weights) != 0)
    return (which.max(weights))
  else
    return (0)
}

PF = function(xl, pots, h, z){
  return(classif_PF(xl, pots, h, sortObjectsByDist(xl, z)));
}
```

Подбор потенциалов выполняется последовательно, следовательно результат всегда одинаковый, время работы всегда одинаковое, но качество от этого страдает.

Код:

```R
SetPots = function(xl, err,h){
  l = nrow(xl)
  n = ncol(xl)
  error = 999
  nerror = 0;
  pots = array(0,l)
  distances = matrix(-1,l,l-1)
  
  for(i in 1:l){
    distances[i,] = sortObjectsByDist(xl[-i,], c(xl[i,1],xl[i,2]))
    print(i)
  }
    for(i in 1:l){
      class = classif_PF(xl[-i,], pots, h, distances[i,])
      
      if(class != c(xl[i,n])){
        pots[i] = pots[i] + 1
        
        nerror = 0;
        for(j in 1:l){
          class = classif_PF(xl[-j,], pots, h, distances[j,])
          if(class != c(xl[j,n])){
            nerror = nerror + 1
          }
        }
        
        if(nerror>=error){
          pots[i] = pots[i] - 1
        }
        else if(nerror <= err){
          i = 1
        }
        else{
          error = nerror
          e = paste("error = ", error);
          print(e)
          print(pots)
        }
      }
     }
  }
```

Карты потенциалов:

![Ну нет ее и все! Отстань!](/PF/PF1111.png)

![Ну нет ее и все! Отстань!](/PF/PF2222.png)

![Ну нет ее и все! Отстань!](/PF/PF3333.png)

Карты классификации:

![Ну нет ее и все! Отстань!](/PF/PF111.png)

![Ну нет ее и все! Отстань!](/PF/PF222.png)

![Ну нет ее и все! Отстань!](/PF/PF333.png)
