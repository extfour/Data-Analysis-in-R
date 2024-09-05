df  <- mtcars
df_numeric  <- df[,c(1,3:7)]

fit  <- lm(mpg ~ hp, df)
summary(fit)

ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values )

new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg  <- predict(fit, new_hp)

predict(fit, new_hp)


##################################

my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)


# Скачайте набор данных - dataframe с двумя количественными переменными
# (вспомните при необходимости, как задавать разделитель и другие параметры функции
#   read.table), постройте линейную регрессию, где - первая переменная - зависимая,
# вторая - независимая. В ответ укажите значения регрессионных коэффициентов сначала
# intercept затем  slope.
#
# Десятичный разделитель - точка. В поле для ответа введите два числа, не округляйте
# значения, например;
#
# 12.434 6.2557

df <- read.table("data/dataset_11508_12.txt")

fit  <- lm(V1 ~ V2, df)
fit$coefficients

# Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. Только для
# бриллиантов класса Ideal (переменная cut) c числом карат равным 0.46 (переменная carat)
# постройте линейную регрессию, где в качестве зависимой переменной выступает price,
# в качестве предиктора - переменная  depth. Сохраните коэффициенты регрессии в переменную
# fit_coef.
#
# Памятка:
# > fit <- lm(mpg ~ disp + wt, mtcars)
# > fit$coefficients # коэффициенты модели
#
# Это задание нужно решить, не используя цикл for().

# df <- diamonds[diamonds$cut == 'Ideal' & diamonds$carat == 0.46,] нужна "," в конце
# чтобы указать выборку строк

df <- subset(diamonds, cut == 'Ideal' & carat == 0.46)
fit  <- lm(price ~ depth, df)
fit_coef <- fit$coefficients

# Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
#
# Если две переменные значимо коррелируют
#   (p - уровень значимости для коэффициента корреляции Пирсона меньше 0.05), то функция
#    строит регрессионную модель, где первая переменная - зависимая, вторая - независимая.
#    Затем создает в dataframe новую переменную с назанием fit, где сохраняет предсказанные
#    моделью значения зависимой переменной. В результате функция должна возвращать исходный
#    dataframe с добавленной новой переменной fit.
#
# Если две переменные значимо не коррелируют,
#   то функция возвращает строчку "There is no sense in prediction"
#
# Примеры работы функции:
#
# > my_df = iris[,1:2] # на вход подаем данные iris только с переменными Sepal.Length и Sepal.Width
# > regr.calc(iris[,1:2]) # переменные значимо не коррелируют
#
# [1] "There is no sense in prediction"
#
# > my_df = iris[,c(1,4)] # на вход подаем данные iris только с переменными Sepal.Length и Petal.Width
# > regr.calc(my_df) # переменные значимо коррелируют
#
#     Sepal.Length Petal.Width      fit
#
# 1            5.1       0.2   4.955345
# 2            4.9       0.2   4.955345
# 3            4.7       0.2   4.955345
# .            .         .     .
# .            .         .     .
#
# Обратите внимание, при проверке вашей функции на вход будут подаваться данные с различными именами колонок.
# Ваша функция должна корректно работать в независимости от имен переменных.
#
# Перед тем как сдавать решение убедитесь, что ваша функция работает корректно на разных данных,
# с разными именами колонок.

library(psych)

regr.calc <- function(df){
  res <- corr.test(df[1], df[2])
  if (res$ci$p < 0.05) {
    fit  <- lm(df[[1]] ~ df[[2]], df)
    df$fit <- fit$fitted.values
    return(df)
  } else {
    return("There is no sense in prediction")
  }
}

regr.calc(iris[,c(1,4)])

# Постройте scatterplot по данным iris, сохранив его в переменную my_plot :
# Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы наблюдений по переменной
# Species.
#
# Если Вы все сделали правильно должен получиться следующий график:

install.packages("png")
library("png")

pp <- readPNG("data/Rplot.png")
plot.new()
rasterImage(pp,0,0,1,1)

# Пожалуйста, сохраняйте график в переменную my_plot.

library(ggplot2)

my_plot <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, colour = Species))+
  geom_smooth(method = "lm")+
  geom_point()

  
