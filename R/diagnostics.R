#
# regression diagnostics
# 
install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
chart.Correlation(swiss)

library(ggplot2)

data(swiss)
str(swiss)



# relationships between all variables
pairs(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point()


# Outliers

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth(method = 'lm')


# Normality of variables distributions

ggplot(swiss, aes(x = Examination)) + 
  geom_histogram()

ggplot(swiss, aes(x = Education)) + 
  geom_histogram()



# В переменной my_vector хранится вектор значений.

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 
               0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167,
               0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 
               0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 
               0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004,
               0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 
               0.061, 0.523)

# Какое преобразование позволяет сделать его распределение нормальным 
# (согласно shapiro.test)?

shapiro.test(my_vector)       # W = 0.9237,  p-value = 0.003216
shapiro.test(log(my_vector))  # W = 0.88094, p-value = 0.0001195
shapiro.test(1/my_vector)     # W = 0.25593, p-value = 1.704e-14
shapiro.test(sqrt(my_vector)) # W = 0.97184, p-value = 0.2745
  
# Выберите все подходящие ответы из списка

# log(my_vector)
# ни одно преобразование не работает
# 1/my_vector
# sqrt(my_vector)                     +
# все преобразования работают



# Функция scale() позволяет совершить стандартизацию вектора, то есть делает его
# среднее значение равным нулю, а стандартное отклонение - единице 
# (Z-преобразование).
# 
# Стандартизованный коэффициент регрессии (β) можно получить, если предикторы и
# зависимая переменная стандартизованы.
# 
# Напишите функцию, которая на вход получает dataframe с двумя количественными
# переменными, а возвращает стандартизованные коэффициенты для регрессионной
# модели, в которой первая переменная датафрейма выступает в качестве зависимой,
# а вторая в качестве независимой.
# 
# Примеры работы функции.
# > beta.coef(mtcars[,c(1,3)])
# -7.036582e-17 -8.475514e-01
# 
# > beta.coef(swiss[,c(1,4)])
# 3.603749e-16 -6.637889e-01 
# 
# Подсказка:
#   
#   ?scale

beta.coef <- function(df){
  c1 <- scale(df[[1]])
  c2 <- scale(df[[2]])
  return(lm(c1 ~ c2, data.frame(c1, c2))$coefficients)
}

beta.coef(swiss[,c(1,4)])

# То, что вы только что сделали, можно сделать с помощью функции lm.beta из 
# библиотеки QuantPsyc! :)



# Напишите функцию normality.test, которая получает на вход dataframe с 
# количественными переменными, проверяет распределения каждой переменной на 
# нормальность с помощью функции shapiro.test. Функция должна возвращать вектор
# с значениями p - value, полученного в результате проверки на нормальность 
# каждой переменной. Названия элементов вектора должны совпадать с названиями
# переменных. 
# 
# Пример работы функции:   
# > normality.test(mtcars[,1:6])
# mpg          cyl          disp         hp           drat         wt 
# 1.228814e-01 6.058338e-06 2.080657e-02 4.880824e-02 1.100608e-01 9.265499e-02 
# 
# > normality.test(iris[,-5])
# Sepal.Length Sepal.Width  Petal.Length Petal.Width 
# 1.018116e-02 1.011543e-01 7.412263e-10 1.680465e-08 
# 
# Опять же, обратите внимание функция должна работать корректно с различным
# количеством переменных и в независимости от их названий.
# 
# Подсказка. Как задать имена элементов вектора:
# > my_vector <- c(1, 2, 3, 4)
# > names(my_vector) <- c("A", "B", "C", "D")
# > my_vector
# 
# A B C D 
# 1 2 3 4



normality.test <- function(df){
  return(sapply(df, shapiro.test)[2,])
}


normality.test(mtcars[,1:6])

# linearity 

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth()

lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)


swiss$Examination_squared <- (swiss$Examination)^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)


anova(lm2, lm1)


swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point(size = 3) + 
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd=1) +
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd=1)


ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3) + geom_hline(y=0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_hline(y=0, col = 'red', lwd = 1)


# independence of errors

ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth()


# Homoscedasticity

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3)


 
# Функция gvlma() из библиотеки gvlma позволяет получить оценку выполнения 
# основных допущений линейной регрессии. В качестве аргумента она принимает 
# объект, в который сохранена модель. Можно задать формулу модели прямо в функции
# gvlma. Чтобы увидеть основные статистики, нужно выполнить команду summary для
# объекта, созданного с помощью функции gvlma.
# 
# Например,
# 
# x <- gvlma(fit)
# # или
# x <- gvlma(Y ~ X, data = mydata)
# summary(x)
# 
# Загрузите себе прикреплённый к этому степу датасет и постройте регрессию, 
# предсказывающую DV по IV. Установите библиотеку gvlma и проверьте, 
# удовлетворяется ли в этой модели требование гомоскедастичности. Введите в поле
# ответа p-значение для теста гетероскедастичности.

install.packages('gvlma')
library('gvlma')

df <- read.csv("data/homosc.csv")
fit <- lm(DV ~ IV, df)
x <- gvlma(fit)
summary(x)



# Errors Normally distributed

ggplot(swiss, aes(x = lm1_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)


ggplot(swiss, aes(x = lm2_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)





# Напишите функцию resid.norm, которая тестирует распределение остатков от модели
# на нормальность при помощи функции shapiro.test и создает гистограмму при помощи
# функции ggplot() с красной заливкой "red", если распределение остатков значимо
# отличается от нормального (p < 0.05), и с зелёной заливкой "green" - если
# распределение остатков значимо не отличается от нормального.
# 
# На вход функция получает регрессионную модель. Функция возвращает переменную,
# в которой сохранен график ggplot.
# 
# В поле для ответа не нужно создавать никаких дополнительных объектов, только
# напишите функцию  resid.norm.
# 
# Пример работы функции:
#   
# > fit <- lm(mpg ~ disp, mtcars)
# > my_plot <- resid.norm(fit)
# > my_plot
# 
# > fit <- lm(mpg ~ wt, mtcars)
# > my_plot <- resid.norm(fit)
# > my_plot
# 
# Для создания гистограммы при помощи функции ggplot требуется dataframe, где
# хранится переменная. Обратите внимание на такие функции как:
#   
# data.frame()
# as.data.frame()

fit <- lm(mpg ~ disp, mtcars)
fit <- lm(mpg ~ wt, mtcars)

resid.norm <- function(fit){
  if(shapiro.test(fit$residuals)$p.value < 0.05){
    fill_color <- 'red'
    
  } else {
    fill_color <- 'green'
  }
  plot <- ggplot(fit$model, aes(x = fit$residuals)) + 
    geom_histogram(binwidth = 4, fill = fill_color, col = 'black')
  return(plot)
}

my_plot <- resid.norm(fit)



# Задача для супергероев!!!
#   
# Ещё одной проблемой регрессионных моделей может стать мультиколлинеарность - 
# ситуация, когда предикторы очень сильно коррелируют между собой. Иногда 
# корреляция между двумя предикторами может достигать 1, например, когда два 
# предиктора - это одна и та же переменная, измеренная в разных шкалах 
# (x1 - рост в метрах, x2 - рост в сантиметрах)  
# 
# Проверить данные на мультиколлинеарность можно по графику pairs() и посчитав
# корреляцию между всеми предикторами c помощью функции cor.
# 
# Напишите функцию high.corr, которая принимает на вход датасет с произвольным
# числом количественных переменных и возвращает вектор с именами двух переменных
# с максимальным абсолютным значением коэффициента корреляции .
# 
# Примеры работы функции:
#   
# > high.corr(swiss)
# [1] "Examination" "Education"
# 
# > high.corr(iris[,-5])
# [1] "Petal.Length" "Petal.Width"
# 
# > x1 <- rnorm(30) # создадим случайную выборку
# > x2 <- rnorm(30) # создадим случайную выборку
# > x3  <- x1 + 5 # теперь коэффициент корреляции x1 и x3 равен единице
# > my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3)
# > high.corr(my_df)
# [1] "var1" "var3"
# 
# 
# 
# Вам могут понадобиться следующие функции: which, dimnames, colnames, rownames,
# diag, abs. Посмотрите справку по ним.
# 
# Подсказки:
#   Далеко не всегда 1 == 1 есть ТRUE! Смотри закрепленный комментарий или можно почитать о проблеме здесь.

x1 <- rnorm(30) # создадим случайную выборку
x2 <- rnorm(30) # создадим случайную выборку
x3  <- x1 + 5 # теперь коэффициент корреляции x1 и x3 равен единице
my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3)

high.corr <- function(df){
  res <- cor(df)
  diag(res) <- 0
  abs_res <- abs(res)
  inxs <- which(abs_res == max(abs_res), arr.ind = TRUE)
  return(rownames(inxs))
}

high.corr(my_df)

