# Линейный дискриминант Фишера

---

Онован на подстановочном алгоритме с предположением, что ковариационные матрицы классов равны. Отсюда следует, что разделяющая поверхность вырождается в прямую. Это условие в plug-in не выполнялось, так как разделяющая поверхность все равно была квадратичной (хоть и приближенной к прямой). Отсюда следует, что ЛДФ должен иметь более высокое качество классификации при одинаковых ковариационных матрицах.

Подстановочный алгоритм имеет вид:

![Ну нет ее и все! Отстань!](/LDF/ldff.png)

```R
LDF = function(xl){
  n = ncol(xl)-1
  l = nrow(xl)
  classes = levels(xl[,n+1])
  m = length(classes)
  
  mus = matrix(0, m, n)
  covs = matrix(0, n, n)
  aprior  = rep(0, m)
  
  for (i in 1:m) {
    class = factor(classes[i], classes)
    xll = xl[xl[,n+1]==class,]
    aprior [i] = length(xll[,1]) / length(xl[,1])
    for (j in 1:n) {
      mus[i,j] = mean(xll[,j])
    }
  }
  
  for (k in 1:l) {
    for (i in 1:n) {
      for (j in 1:n) {
        c = xl[k,n+1]
        covs[i,j] = covs[i,j] + (xl[k,i] - mus[c,i]) * (xl[k,j] - mus[c,j])
      }
    }
  }
  covinv = solve(covs/l)
  
  
  classificationmap(function(x) f(x, classes, aprior , mus, covinv), xl)
}
```
Карта классификации для ирисов Фишера:

![Ну нет ее и все! Отстань!](/LDF/ldf11.png)
