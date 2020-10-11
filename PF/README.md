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

Карта потенциалов:

Карта классификации:
