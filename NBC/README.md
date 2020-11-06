# Наивный Байесовский классификатор

---

Простой вероятностный классификатор, основанный на применении теоремы Байеса со строгими (наивными) предположениями о независимости. Достоинством наивного байесовского классификатора является малое количество данных, необходимых для обучения, оценки параметров и классификации.

Код:

```R
NBS = function(z, xl){
  mu = fmu(xl)
  sigma = fsigma(xl)
  aprior = faprior(xl)
  l = dim(mu)[1]
  lyambda = seq(1,l)
  
  max = -1
  best = aprior[1,1]
  
  for(i in 1:l){
    class = aprior[i,1]
    
    r = log( lyambda[i] * aprior[i,2] )
    
    l = ftight(z,class,mu,sigma)
    lr = l + r
    
    if(lr > max){
      max = lr
      best = class
    }
  }
  
  ans = c(best, exp(max))
  
  return(ans)
}
```

![Ну нет ее и все! Отстань!](/NBC/NNBC1.png)

![Ну нет ее и все! Отстань!](/NBC/NNBC4.png)

![Ну нет ее и все! Отстань!](/NBC/NNBC3.png)
