# ML1
Репозиторий для практических работ по СМПР. (2020)

---

| ***Метрические алгоритмы классификации*** |      ***Условия***      | ***LOO*** |  ***Ссылка*** |
|---------|:------------------------------:|:--------:|--------:|
| Ближайшего соседа                          | -              | -       | [подробнее](https://github.com/Vector232/ML1/tree/master/1NN)  |
| k ближайших соседей                        | k = 6          | 0.033 % |[подробнее](https://github.com/Vector232/ML1/tree/master/kNN)   |
| k взвешенных ближайших соседей             | k = 6, q = 0.9 | 0.033 % | [подробнее](https://github.com/Vector232/ML1/tree/master/kwNN) |
| [Преимущество **kNN** перед **kNN**  ](https://github.com/Vector232/ML1/tree/master/results)                                           |
| Парзеновского окна - ядро Епанечникова     | h = 0.4        | 0.04 % | [подробнее](https://github.com/Vector232/ML1/tree/master/PW) |
| Парзеновского окна - квартическое  ядро    | h = 0.4        | 0.04 % | [подробнее](https://github.com/Vector232/ML1/tree/master/PW) |
| Парзеновского окна - треугольное ядро      | h = 0.4        | 0.04 % | [подробнее](https://github.com/Vector232/ML1/tree/master/PW) |
| Парзеновского окна - Гауссовское ядро      | h = 0.1        | 0.04 % | [подробнее](https://github.com/Vector232/ML1/tree/master/PW) |
| Потенциальных функций - ядро Епанечникова  |  {p} = 17      | 0.06 % | [подробнее](https://github.com/Vector232/ML1/tree/master/PF) |
| Потенциальных функций - квартическое  ядро |  {p} = 16      | 0.07 % | [подробнее](https://github.com/Vector232/ML1/tree/master/PF) |
| Потенциальных функций - Гауссовское ядро   |  {p} = 3       | 0.13 % | [подробнее](https://github.com/Vector232/ML1/tree/master/PF) |
| Парзеновского окна + STOLP - Гауссовское ядро | h = 0.1     | 0.026 %| [подробнее](https://github.com/Vector232/ML1/tree/master/STOLP) |
| ***Байесовские алгоритмы классификации***     |***Условия***| ***LOO*** |  ***Ссылка*** |
|Линии уровня                                   | - | - | [подробнее](https://github.com/Vector232/ML1/tree/master/lines) |
|Наивный Байесовский классификатор              |lambda = (1,1,1)| 0.04 % | [подробнее](https://github.com/Vector232/ML1/tree/master/NBC) |
|PlugIn                                         |lambda = (1,1,1)| 0.04 % | [подробнее](https://github.com/Vector232/ML1/tree/master/PlugIn) |
|LDF                                            |         -      | 0.04 %  | [подробнее](https://github.com/Vector232/ML1/tree/master/LDF) |
| ***Линейные алгоритмы классификации***     |***Условия***| ***LOO*** |  ***Ссылка*** |
|ADALINE                                     |         -      | -       | [подробнее](https://github.com/Vector232/ML1/tree/master/ADALINE) |
|Логистическая	регрессия                    |         -      | -       | [подробнее](https://github.com/Vector232/ML1/tree/master/LC) |
|SVM                                         |         -      | -       | [подробнее](https://github.com/Vector232/ML1/tree/master/SVM) |
Карты классификаций:

![Ну нет ее и все! Отстань!](/1NN/1NN(2).png)

![Ну нет ее и все! Отстань!](/kNN/6NN(2).png)

![Ну нет ее и все! Отстань!](/kwNN/kwNN(3).png)

Парзеновское окно:

![Ну нет ее и все! Отстань!](/PW/CE(PW).png)

Парзеновское окно:

![Ну нет ее и все! Отстань!](/PW/CC(PW).png)

Парзеновское окно:

![Ну нет ее и все! Отстань!](/PW/TC(PW).png)

Парзеновское окно:

![Ну нет ее и все! Отстань!](/PW/GC(PW).png)

STOLP:

![Ну нет ее и все! Отстань!](/STOLP/STOLPendCM.png)

NNBC:

![Ну нет ее и все! Отстань!](/NBC/NNBC111.png)

PlugIn:

![Ну нет ее и все! Отстань!](/PlugIn/PI11.png)

LDF:

![Ну нет ее и все! Отстань!](/LDF/ldf11.png)
