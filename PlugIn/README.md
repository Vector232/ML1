# PlugIn

---

Оценим параметры функций правдоподобия по частям обучающей выборки  для каждого класса. Затем эти выборочные оценки подставим в оптимальный байесовский классификатор. Получим байесовский нормальный классификатор, который называется также подстановочным.

коэффициенты разделяющей кривой:

![На обеде!](/PlugIn/PIf220.gif)

![На обеде!](/PlugIn/PIRP.png)

```R
PI = function(xl){
  n = ncol(xl)-1
  l = nrow(xl)
  classes = levels(xl[,n+1])
  
  
  classes = levels(xl[,n+1])
  l = length(classes)
  
  mus = matrix(0, l, n)
  stds = matrix(0, l, n)
  aprior = rep(0, l)
  lyambda = rep(1, l)
  for (i in 1:l) {
    c = factor(classes[i], classes)
    
    xll = xl[xl[,n+1]==c,]
    aprior[i] = length(xll[,1]) / length(xl[,1])
    for (j in 1:n) {
      mus[i,j] = mean(xll[,j])
      stds[i,j] = sqrt(var(xll[,j]))
    }
  }
  
  covar = list()
  for (c in 1:length(classes)) {
    xll = xl[xl[,n+1]==classes[c],1:n]
    covar[[c]] = cov(xll)
  }
  
  classificationmap(function(x) f(x, classes, aprior, mus, covar, lyambda), xl)
}
```

Плотности распределения при lyambda = (1,1,1):

![На обеде!](/PlugIn/PI1.png)

Карта классификации для ирисов Фишеоа:

![На обеде!](/PlugIn/PI11.png)

![На обеде!](/PlugIn/PGL.png)

Рассмотрим другие наборы данных при которых разделяющая кривая принимает вид:

**Эллипс ->**

![На обеде!](/PlugIn/PI31.png)

**Порабола ->**

![На обеде!](/PlugIn/PI41.png)

**Гипербола ->**

![На обеде!](/PlugIn/PI51.png)
