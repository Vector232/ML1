# Логистическая регрессия

---

Логистическая регрессия - это статистический метод для анализа набора данных, в котором есть одна или несколько независимых переменных, которые определяют результат. Результат измеряется с помощью дихотомической переменной (в которой есть только два возможных результата). Он используется для прогнозирования двоичного результата (1/0, Да / Нет, Истина / Ложь) с учетом набора независимых переменных.

```R
GS = function(xl, eta, lyambda){
  n = ncol(xl)
  l = nrow(xl)
  
  w = matrix(c(runif(n-1, -1/(2*(n-1)), 1/(2*(n-1)))), 1, 3)
  obj <- xl[,-n]
  classes <- xl[, n]
  Q = sum(sapply(1:l, function(i) LOSS(margin(w, obj[i,], classes[i]))))
  
  ANSQ = matrix(Q, 1, 1)
  
  c = 0
  
  while(TRUE){
    c = c + 1
    
    margins = sapply(1:l, function(i) margin(w[c,],obj[i,],classes[i]))
    err = which(margins < 0)
    
    if(length(err) == 0) break
    
    if (length(err) > 0) rand = sample(err, 1)
    else rand = sample(1:l, 1)
    
    eps = LOSS(margin(w[c,], obj[rand,], classes[rand]))
    
    eta = 1 / (obj[rand,] %*% obj[rand,])^2
    if (length(err) == 0) eta = eta / 2
    
    w = rbind(w, f(w[c,], obj[rand,], classes[rand], eta))
    
    oldQ = Q
    Q = (1 - lyambda) * Q + lyambda * eps
    ANSQ = rbind(ANSQ, Q)
    
    if (abs(oldQ - Q) / max(oldQ, Q) <= 0.0001)  break
    else if (c == 30000) break
  }
  w <- cbind(w, ANSQ)
  return(w)
}
```

