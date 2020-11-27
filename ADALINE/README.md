![Ну нет ее и все! Отстань!](/LC/ADALINEiter2.png)

```R
LOSS = function(x){
  return((1-x)^2)
}

sigmoid = function(x){
  return(1/(1 + exp(-x)))
} 

f = function(w, obj, class, eta){
  return(w - c(eta) * (w %*% obj - class) %*% obj)
}
```

![Ну нет ее и все! Отстань!](/LC/ADALINESCREENSHOT.PNG)
