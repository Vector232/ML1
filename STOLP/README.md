# STOLP

---

Алгоритм отбора эталонных объектов для метрического классификатора. Пусть у нас задана обучающая выборка, и некоторая метрика, такая, что выполняется гипотеза компактности. Если классифицировать объекты метрическим классификатором, то время затрачиваемое на это для каждого объекта, пропорционально размеру обучающей выборке.

Типы объектов:

- Эталонные — типичные представители классов. Если классифицируемый объект близок к эталону, то, скорее всего, он принадлежит тому же классу.
- Неинформативные — плотно окружены другими объектами того же класса. Если их удалить из выборки, это практически не отразится на качестве классификации.
- Выбросы — находятся в окружении объектов чужого класса. Как правило, их удаление только улучшает качество классификации.

Код:

```R
stolp = function(xl, classes,errors) {
  n = length(classes)
  #считаем все риски(отступы)
  risk = rep(0, n)
  for (i in 1:n){
    risk[i] = risk(xl, classes, xl[i,], classes[i])
    print(risk[i])
  }
  
  
  
  
  plot(1:n, sort(risk), col="black", bg="blue",pch=20,main = "Риски",ylab = "Риск ", xlab = "Данные")
  lines(1:n, sort(risk), lwd = 2, col = "black")
  
  #удаляем объекты с рисками меньше нуля, как неинформативные
  badpoints = which(risk < 0)
  pointsWE=xl[-badpoints,]
  classes = classes[-badpoints]
  n = n - length(badpoints)
  etalone = data.frame()
  #объекты с максимальным риском являются эталонным
  for (class in unique(classes)) {
    ind = which(class == classes )
    risk = sapply(ind, function(i) risk(pointsWE, classes, pointsWE[i,], class))
    maxrisk = ind[which.max(risk)]
    etalone=rbind(etalone, pointsWE[maxrisk,])
    pointsWE=pointsWE[-maxrisk,]
    classes=classes[-maxrisk]
    n = n - 1
  }
  # пересчитали величины риска для всех объектов и среди объектов каждого класса, распознанных неправильно, 
  # выбрать объекты с максимальной величиной риска и добавить их к эталонам
  names(etalone) = names(xl)
  while(n!=length(etalone)){
    count=0
    risk = c()
    index = c()
    for(i in 1:n)
    {
      m = risk(etalone, etalone[,3], pointsWE[i,], classes[i])
      if(m<=0){
        count=count+1;
        risk = c(risk, m)
        index = c(index, i)
      }
    }
    if( count < errors ) break
    
    #удаляем шумовые элементы
    minrisk = index[which.min(risk)]
    etalone = rbind(etalone, pointsWE[minrisk,])
    pointsWE = pointsWE[-minrisk,]
    classes = classes[-minrisk]
    n = n - 1
    
  }
  plot(pointsWE[,1:2],col = colors[classes], pch = 21, asp = 1, main = "STOLP", ylab = "y ", xlab = "x")
  points(etalone[,1:2], bg = colors[etalone[,3]], pch = 21)
}
```

Отступы для Парзеновского окна с Гауссовским ядром при **h = 0.1** :

![Ну нет ее и все! Отстань!](/STOLP/risk.png)

STOLP и Карта классификации:

![Ну нет ее и все! Отстань!](/STOLP/STOLP.png)

![Ну нет ее и все! Отстань!](/STOLP/STOLPendCM.png)

При уменьшении начальной выборки до нескольких эталонных объектов, ожидаемо увеличилась скорость работы алгоритма Парзеновского окна и незначительно увеличилось количество неправильно классифицируемых объектов.
