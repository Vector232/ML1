# Пример лучшей работы kwNN чем kNN

У каждого из этих методов есть преимущества и недостатки. **kNN** является одним из самых простых алгоритмов классификации и не самым эффективным.

---

Создана уникальная выборка:

```R
xl = matrix(0,16,3);

xl[1,] = c(3,4,1)
xl[2,] = c(4,5,1)
xl[3,] = c(2,8,1)
xl[4,] = c(6,6,1)
xl[5,] = c(3,1,1)
xl[6,] = c(4,8,2)
xl[7,] = c(5,9,2)
xl[8,] = c(5,7,2)
xl[9,] = c(5,8,2)
xl[10,] = c(5,11,2)
xl[11,] = c(7,13,3)
xl[12,] = c(5,12,3)
xl[13,] = c(9,11,3)
xl[14,] = c (6,10,3)
xl[15,] = c(8,12,3)
xl[16,] = c(6,5,3)
```

На ней были сравнены результаты работы **kwNN** и **kNN**:

Результаты **kNN**:

![Ну нет ее и все! Отстань!](/results/LOO(k)kNN(2).png) ![Ну нет ее и все! Отстань!](/results/kNN(res).png)

Результаты **kwNN**:

![Ну нет ее и все! Отстань!](/results/LOO(k)kwNN(2).png) ![Ну нет ее и все! Отстань!](/results/kwNN(res).png)

На уникальной выборке алгоритм **kwNN** оказался эффективнее.