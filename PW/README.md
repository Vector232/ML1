# Метод парзеновского окна

**Метод парзеновского окна** — метод байесовской классификации, основанный на непараметрическом восстановлении плотности по имеющейся выборке.

В основе подхода лежит идея о том, что плотность выше в тех точках, рядом с которыми находится большое количество объектов выборки. 

---

Код:

```R
core_1 = function(r){
  
  if(r <= 1){
    return((3/4)*(1-r^2))
  }
  else
    return(0)
}

core_2 = function(r){
  
  if(r <= 1){
    return((15/16)*(1-r^2)^2)
  }
  else
    return(0)
}

core_3 = function(r){
  
  if(r <= 1){
    return((1-abs(r)))
  }
  else
    return(0)
}

classif_PW = function(xl, h, orderedXl, core = core_1) {
  l = dim(xl)[1];
  n = dim(xl)[2];
  
  weights = table(xl[1:l, n]);
  weights[1:dim(weights)] = 0;
  
  for (i in 1:l) {
    weights[xl[i, n]] = weights[xl[i, n]] + core(orderedXl[i] / h);
  }
  
  if (max(weights) != 0)
    return (which.max(weights))
  else
    return (0)
}

PW = function(xl, h, z){
  return(classif_PW(xl, h, sortObjectsByDist(xl, z)));
}
```

Использованы ядра:

[***Подробнее о подборе оптимального h***](https://github.com/Vector232/ML1/tree/master/PWLOO)

core_1 - Ядро Епанечникова при h = 0.4

![Ну нет ее и все! Отстань!](/PW/CE(PW).png)

core_2 - Квартическое  ядро при h = 0.4

![Ну нет ее и все! Отстань!](/PW/CC(PW).png)

core_3 - Треугольное ядро при h = 0.4

![Ну нет ее и все! Отстань!](/PW/TC(PW).png)

core_4 - Гауссовское ядро при h = 0.4

![Ну нет ее и все! Отстань!](/PW/GC(PW).png)

После применения **LOO** определено оптимальное значение **h = 0.4** для первых трех ядер и **h = 0.1** для Гауссовского.
