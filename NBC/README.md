# Наивный Байесовский классификатор

---

Простой вероятностный классификатор, основанный на применении теоремы Байеса со строгими (наивными) предположениями о независимости. Достоинством наивного байесовского классификатора является малое количество данных, необходимых для обучения, оценки параметров и классификации.

Код:

```R
fsigma = function(xl){ # все сред квадрат отклонения
  l = ncol(xl)
  ans = data.frame()
  for(i in levels(xl[,l])){
    signs = xl[xl[,l] == i, ]
    m = nrow(signs)
    
    isdisp = vector()
    
    for(j in 1:(l-1)){
      dmu = (sum(signs[,j])/m)^2
     
      muford = sum(signs[,j]^2)/m
      
      
      isdisp = cbind(isdisp, sqrt(muford-dmu))
    }
    
    ans = rbind(ans, c(isdisp, i))
  }
  
  return(ans)
}

fmu = function(xl){ #все мат ожидания
  l = ncol(xl)
  ans = data.frame()
  for(i in levels(xl[,l])){
    signs = xl[xl[,l] == i, ]
    m = nrow(signs)
    
    ismu = vector()
    
    for(j in 1:(l-1)){
      ismu = cbind(ismu, sum(signs[,j])/m)
    }
    
    ans = rbind(ans, c(ismu, i))
  }
  return(ans)
}

faprior = function(xl){ # априорные вероятности
  n = nrow(xl)
  l = ncol(xl)
  
  ans = data.frame(table(xl[,l]) / n)
  
  return(ans)
}

ftight = function(z, class, mu, sigma){ # плотность объекта
  l = ncol(z)
  k = ncol(mu)
  
  ismu = mu[mu[,k]==class,]
  
  k = ncol(sigma)
  issigma = sigma[sigma[,k]==class,]
  
  tight = 0
  
  for(i in 1:l){
    tight = tight + log(formula(z[,i],ismu[,i],issigma[,i]))
  }
  return(tight)
}

formula = function(z, mu, sigma){ #формула плотности для наив норм баес классиф
  mu = as.numeric(mu)
  sigma = as.numeric(sigma)
  
  ans = (1/(sigma*sqrt(2*pi)))*(exp(-(((z-mu))^2/(2*sigma^2))))
  
  return(ans)
}

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

xl = iris[,3:5]
i = 5
ans = NBS(xl[i,1:2],xl[-i,])
print(ans)
```

![Ну нет ее и все! Отстань!](/NBC/NNBC1.png)

![Ну нет ее и все! Отстань!](/NBC/NNBC4.png)

![Ну нет ее и все! Отстань!](/NBC/NNBC3.png)
