# multiple linear regression


# numeric predictors

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)


fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)


confint(fit2)

# Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
#   
# x_1  -  числовой вектор
# x_2 - числовой вектор
# y - числовой вектор с пропущенными значениями.
# 
# Теперь — ﻿самое интересное. На первом этапе, используя только наблюдения, 
# в которых нет пропущенных значений, мы построим регрессионную модель 
# (без взаимодействий), где  y — зависимая переменная, x_1 и x_2 — независимые 
# переменные. Затем, используя построенную модель, мы заполним пропущенные 
# значения предсказаниями модели.
# 
# Функция должна возвращать dataframe c новой переменной  y_full. Сохраните в нее
# переменную y, в которой пропущенные значения заполнены предсказанными значениями
# построенной модели.
# 
# > test_data <- read.csv("data/fill_na_test.csv")
# > fill_na﻿(test_data)
# x_1 x_2  y   y_full
# 1    9  22 14 14.00000
# 2   14  35  9  9.00000
# 3   10  45 NA 13.12727
# 4    6  35 NA 13.30132
# 5   11  36 16 16.00000
# 6    5  27 11 11.00000
# 7    9  34 NA 12.83632
# 8    6  25 NA 12.90783
# 9   11  38 13 13.00000
# 10  12  23 NA 11.97784

df <- read.csv("data/fill_na_test.csv")

fill_na <- function(df){
  
  fit <- lm(df[[3]] ~ df[[1]] + df[[2]], data = df)
  
  # если модель строилась по усечённым данным (в аргумент data передавались 
  # только валидные данные), то в fitted.values не будет значений для тех данных,
  # для которых заранее не был известен ответ.
  # df$y_full <- ifelse(is.na(df[[3]]), fit$fitted.values, df[[3]])
  
  # y <- predict(fit, df[[3]]) # основаня проблема в том что надо передавать весь df
  # fit <- lm(y ~ x_1 + x_2, df) #тоже работает, но завязано на названия столбцов
  y_full <- predict(fit, df)
  df$y_full <- ifelse(is.na(df[[3]]), y_full, df[[3]])
  return(df)
}
fill_na(df)

# В переменной df сохранен subset данных mtcars только с переменными 
# "wt", "mpg", "disp", "drat", "hp". Воспользуйтесь множественным регрессионным 
# анализом, чтобы предсказать вес машины (переменная "wt"). Выберите такую комбинацию
# независимых переменных (из "mpg", "disp", "drat", "hp"), чтобы значение 
# R^2 adjusted было наибольшим. Взаимодействия факторов учитывать не надо. 
# 
# Выполните все операции по сравнению моделей на вашем компьютере.
# В поле для ответа сохраните в переменную  model регрессионную модель с 
# оптимальной комбинацией предикторов!
df <- mtcars[, c("wt", "mpg", "disp", "drat", "hp")]
model <- lm(wt ~ mpg + disp + drat + hp, df)
model <- lm(wt ~ mpg + disp + hp, df)
summary(model)

# Воспользуйтесь встроенным датасетом attitude, чтобы предсказать рейтинг (rating)
# по переменным complaints и critical. Каково t-значение для взаимодействия двух 
# факторов?
#   
# Разделителем целой и дробной части в ответе должна быть запятая!
fit <- lm(rating ~ complaints * critical, attitude)
summary(fit)


# categorical predictors

hist(swiss$Catholic, col = 'red')

swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ religious*Examination, data = swiss)
summary(fit4)

# plots

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')


#

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)

# В этом примере будем работать с хорошо вам известным встроенным датасетом mtcars.
# Переменная am говорит о том, какая коробка передач используется в машине: 
# 0 - автоматическая, 1 - ручная.
# 
# Сделаем эту переменную факторной.
# mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
# 
# Теперь постройте линейную модель, в которой в качестве зависимой переменной 
# выступает расход топлива (mpg), а в качестве независимых - вес машины (wt) и 
# коробка передач (модифицированная am), а также их взаимодействие. 
# Выведите summary этой модели.
# 
# Что отражает значение intercept в данной модели?

df <- mtcars
df$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
fit <- lm(mpg ~ wt * am, df)
summary(fit)

# Средний расход топлива у машин с автоматической коробкой передач          
# Средний расход топлива у машин с нулевым весом и ручной коробкой передач  
# Расход топлива у машин с автоматической коробкой передач и нулевым весом  +
# Расход топлива у машин со средним весом                                 
# Расход топлива у машин с нулевым весом

# В этой задаче снова нужно использовать модель из предыдущей задачи и её summary.
# Какие утверждения мы можем сделать на основе данной модели?
# Обратите внимание на то, что чем выше значение mpg (miles per gallon), тем ниже 
# будет расход топлива (на одном галлоне бензина машина сможет проехать большее).

# 1. У машин с ручной коробкой передач расход топлива ниже
# 2. В машинах с ручной коробкой передач с увеличением веса растёт расход топлива, 
#   а с автоматической - наоборот
# 3. Вес автомобиля не влияет на расход топлива
# 4. В машинах с ручной коробкой передач вес сильнее влияет на расход топлива
# 5. У машин с ручной коробкой передач расход топлива выше
# 6. В машинах с автоматической коробкой передач вес не влияет на расход топлива, 
#   а с ручной - влияет

ggplot(df, aes(x = wt, y = mpg, fill=am)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

# (1, 4)

# Визуализируйте взаимодействие переменных wt и am, дополнив код, приведённый в 
# задании:
#   Ось x - переменная wt
#   Ось y - переменная mpg
#   Цвет регрессионных прямых - переменная am

mtcars$am <- factor(mtcars$am)

my_plot <- ggplot(mtcars, aes(x = wt, y = mpg, color=factor(am))) +
  geom_smooth(method = 'lm')



# model comparison

rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)


# model selection

optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)


# Сейчас мы поработаем со встроенным датасетом attitude. Рассмотрим две модели
# 
# model_full <- lm(rating ~ ., data = attitude) 
# model_null <- lm(rating ~ 1, data = attitude)
# 
# model_full - модель, которая предсказывает значение переменной рейтинг (rating)
# в зависимости от всех остальных переменных в данном датасете.
# 
# model_null - модель, в которой нет ни одного предиктора, а есть только intercept.
# Значение intercept - это просто среднее значение зависимой переменной. 
# Соответственно, модель предоставляет нам информацию только о том, отличается ли
# это среднее от нуля.
# 
# Как говорилось в лекции, функция step позволяет нам подобрать модель с оптимальным
# количеством предикторов. С помощью аргумента scope мы можем задать пространство
# моделей с разным числом предикторов, в котором будет происходить поиск оптимального
# набора предикторов. Самый простой путь - задать границы возможных моделей с помощью
# нулевой и полной моделей.
# 
# scope = list(lower = model_null, upper = model_full)
# Аргумент direction позволяет задать направление поиска.
# 
# Первый аргумент (object) задаёт начальную модель, с которой начинается поиск.
# Обратите внимание на то, что при разных значениях аргумента direction нужно
# использовать разные начальные модели.
# 
# Функция step возвращает оптимальную модель.
# 
# Итак, задача! C помощью функции step найдите оптимальную модель для предсказания
# rating в датасете attitude. Model_full и model_null уже созданы. Сохраните команду
# с функцией step в переменную ideal_model.

model_full <- lm(rating ~ ., data = attitude)
model_null <- lm(rating ~ 1, data = attitude)
scope = list(lower = model_null, upper = model_full)
ideal_model <-  step(model_full, scope, direction = 'backward')

# Сравните полную модель из предыдущего степа и оптимальную модель с помощью
# функции anova. Введите получившееся F-значение.
# 
# Разделителем дробной и целой части в ответе должна быть запятая.

anova(model_full, ideal_model)

# Напоследок потренируемся в эффективном написании формул. В этой задаче будем
# работать со встроенным датасетом LifeCycleSavings. Попытаемся предсказать
# значение sr на основе всех остальных переменных в этом датасете. Вспомните
# способы сокращения формул и напишите команду, которая создаёт линейную регрессию
# с главными эффектами и всеми возможными взаимодействиями второго уровня. 
# Сохраните модель в переменную model.

model <- lm(sr ~ (.)^2, data = LifeCycleSavings)
