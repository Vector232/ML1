# Метод **k** взвешенных ближайших соседей (kwNN)
Метод kwNN отличается от kNN тем, что вес ближайших соседей зависит не от ранга соседа, а от расстояния до классифицируемого объекта.

---

Код:

```R
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
```

Карта классификации для **kwNN** при **k** = 6 и **q** = 0.8 ([подробнее о подборе оптимального k и q](https://github.com/Vector232/ML1/tree/master/kwNNLOO)).

![Ну нет ее и все! Отстань!](/kwNN/kwNN.png)

kwNN имеет лучшее качество классификации чем kNN. Особенно заметно это на границах.

![Ну нет ее и все! Отстань!](/kwNN/kwNNvskNN(kNN).png)

![Ну нет ее и все! Отстань!](/kwNN/kwNNvskNN(kwNN).png)
