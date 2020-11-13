# Наивный Байесовский классификатор

---

Простой вероятностный классификатор, основанный на применении теоремы Байеса со строгими (наивными) предположениями о независимости. Достоинством наивного байесовского классификатора является малое количество данных, необходимых для обучения, оценки параметров и классификации.

Код:

```R
NBC = function(xl){
  n = ncol(xl)-1
  l = nrow(xl)
  classes = levels(xl[,n+1])
  m = length(classes)
  
  
  mus = matrix(0, m, n)
  stds = matrix(0, m, n)
  aprior = array(0, m)
  
  for (i in 1:m) {
    c = factor(classes[i], levels=classes)
    xll = xl[xl[,n+1]==c,]
    aprior[i] = length(xll[,1]) / length(xl[,1])
    for (j in 1:n) {
      mus[i,j] = mean(xll[,j])
      stds[i,j] = sqrt(var(xll[,j]))
    }
  }
  
  classificationmap(function(x) f(x, classes, aprior, mus, stds), xl)
}
```

![Ну нет ее и все! Отстань!](/NBC/NNBC1.png)

![Ну нет ее и все! Отстань!](/NBC/NNBC4.png)

Рассмотрим влияние lyambda:

Все лямбды равны 1: 

![Ну нет ее и все! Отстань!](/NBC/NNBC111.png)

Ламбды: первый класс - 50000000, второй - 0.0001, третий - 0.0001:

![Ну нет ее и все! Отстань!](/NBC/NNBC222.png)
