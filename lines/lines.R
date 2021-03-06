#считает плотность вероятностного распределения для точки
norm = function(x, y, mu, sigma) {
  x = matrix(c(x, y), 2, 1)
  N = ( exp(-0.5 * t(x - mu) %*% solve(sigma) %*% x - mu) ) / sqrt((2 * pi) ^ 2 * det(sigma))
  return(N)
}

a = matrix(c(1,0,0,10), 2, 2) 
s=7;
x=seq(-s, s, 0.1)
y=seq(-s, s, 0.1)
#хранит плотности распределения точек
prob_disp <- data.frame()

for(i in x){
  for(j in y){
    z = c(i, j)
    plot = norm(i,j,0,a)
    prob_disp <- rbind(prob_disp, c(i, j, plot))
  }
}

#отрисовка линий уровня двумерной нормальной плотности распределения
fig <- plot_ly(x = prob_disp[,1],
               y = prob_disp[,2],
               z = prob_disp[,3], type = "contour",
               contours = list(showlabels = TRUE))
fig <- fig %>% colorbar(title = "Плотность \nраспределения")

print(fig)
