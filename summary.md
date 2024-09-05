# t-Критерий Стьюдента для независимых выборок

если первая переменная количественная, а вторая фактор
    
t.test(Var1 ~ Var2, data) 

если обе переменные количественные

t.test(data$Var1, data$Var2) 


# t-Критерий Стьюдента для зависимых выборок

t.test(data$Var1, data$Var2, paired = T)


# Проверка на нормальность распределения

shapiro.test(Var1)

проверка на нормальность распределения переменной Var1
но не удобно когда есть группирующая факторная переменная
 

Поможет функция by(), которая применяет различные функции на каждом уровне фактора.  

by(iris$Sepal.Length, INDICES = iris$Species, shapiro.test) - Проверка на нормальность переменной 

Sepal.Length в трех разных группах в соответствии с переменной Species


# Проверка на гомогенность дисперсий

Критерий Бартлетта bartlett.test(mpg ~ am, mtcars)

cor.test(mtcars$mpg, mtcars$disp) # Расчет корреляции Пирсона 



cor.test(~ mpg + disp, mtcars) # запись через формулу


# Памятка

cor.test(mtcars$mpg, mtcars$disp, method = "spearman") # Расчет корреляции Спирмена 

cor.test(mtcars$mpg, mtcars$disp, method = "kendall") # Расчет корреляции Кендала 

cor(iris[, -5]) # построение корреляционной матрицы

fit <- lm(mpg ~ disp, mtcars) # построение линейной регрессии 

fit$coefficients # коэффициенты регрессии 

fit$fitted.values # предсказанные значения зависимой переменной 
 
При наличии одинаковых значений в переменных расчет непараметрических корреляций будет сопровождаться предупреждением о невозможности рассчитать точное значение p - value.

Если в ваших данных есть одинаковые наблюдения, но вы хотите рассчитать непараметрическую корреляцию, используйте функцию spearman_test  из пакета coin

library(coin)
spearman_test(~ mpg + disp, mtcars)

Обратите внимание на различия в графиках. То что в первом aes() будет распространяться на все слои. А то, что в aes() конкретного geom - только на него.


ggplot(mtcars, aes(mpg, disp, col = factor(am)))+
  geom_point()+
  geom_smooth()



ggplot(mtcars, aes(mpg, disp))+
  geom_point(aes(col = factor(am)))+
  geom_smooth()

ggplot(mtcars, aes(mpg, disp))+
  geom_point()+
  geom_smooth(aes(col = factor(am)))