# PlugIn

---

Оценим параметры функций правдоподобия по частям обучающей выборки  для каждого класса. Затем эти выборочные оценки подставим в оптимальный байесовский классификатор. Получим байесовский нормальный классификатор, который называется также подстановочным.

```R
PI = function(z, xl){
  mu = fmu(xl)
  aprior = faprior(xl)
  l = dim(mu)[1]
  
  lyambda = c(1, 1, 1)
  max = -1
  best = aprior[1,1]
  
  for(i in 1:l){
    
    class = aprior[i,1]
    
    x = xl[c(xl[,3])==i,]
    M = fnewmatrix(x , mu[i,])
   
    tight = frasp(z, mu[i, 1:l-1], M)
    
    lr = lyambda[i] * aprior[i, 2] * tight 
    
    if(lr > max){
      max = lr
      best = class
    }
  }
  if(max < (-0.9999)){
    best = 4
  }
  
  ans = c(best, max)
  
  return(ans)
}
```
