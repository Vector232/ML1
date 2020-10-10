# Метод **k** взвешенных ближайших соседей (kwNN)

Метрический алгоритм для автоматической классификации объектов или регрессии. ближайшим к нему объектам, значения которых уже известны. Алгоритм может быть применим к выборкам с большим количеством атрибутов (многомерным).

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

![Ну нет ее и все! Отстань!](/kwNN/kwNN(3).png)
