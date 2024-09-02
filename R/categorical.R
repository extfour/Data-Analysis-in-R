# Categorical data

df <- read.csv("grants.csv")

str(df)


df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded"))


# 1d Table 
t1 <- table(df$status)
t1

dim(t1)


# 2d Table
t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)

dim(t2)

prop.table(t2)

prop.table(t2, 1)
prop.table(t2, 2)


# 3d Table
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

dim(t3)

# К частям таблицы можно обращаться так же, как и к матрицам.
# HairEyeColor - таблица с данными, встроенными в R. Посмотрите на неё в R. 
# Команда dimnames(HairEyeColor) позволит нам посмотреть, какие измерения есть 
# в этой таблице и как они называются. Например, чтобы обратиться к части таблицы,
# в которой хранятся данные только о мужчинах, нам нужно выполнить следующую 
# команду: 
#   
#   HairEyeColor[ , ,'Male']
# 
# Ваша задача в переменную red_men сохранить долю рыжеволосых (Red) от общего 
# числа голубоглазых мужчин.
# 
# Обратите внимание, что нужны не проценты, а просто доля, то есть десятичная 
# дробь  (например, не 10%, а 0.1).

red_men <- prop.table(HairEyeColor[, , 'Male'], 2)["Red", "Blue"]

# С таблицами, как и с матрицами, можно совершать разные арифметические 
# операции, например, суммировать все элементы таблицы.
# 
# Напишите число зеленоглазых женщин в наборе данных HairEyeColor.

sum(HairEyeColor[, 'Green', 'Female'])


# plots

barplot(t1)
barplot(t2)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)

# Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только 
# у женщин из таблицы HairEyeColor. По оси X должен идти цвет волос, цвет 
# столбиков должен отражать цвет глаз. По оси Y - количество наблюдений.
# 
# Чтобы построить столбчатую диаграмму в ggplot, вам нужно подключить нужный 
# пакет, затем преобразовать таблицу HairEyeColor в data frame:
#   
#   mydata <- as.data.frame(HairEyeColor)
# 
# Постройте график на основе предложенного кода, сохранив его в переменную obj.
# Укажите, чему равен аргумент data, что должно находиться в aes(). Изучите 
# справку по geom_bar(), чтобы узнать, чему должен равняться аргумент position 
# для отображения цвета глаз в виде соседних столбиков, также вам может быть 
# полезна эта памятка. Там же вы найдёте ответ на вопрос, за что отвечает 
# аргумент stat. С помощью scale_fill_manual мы говорим графику, что мы хотим, 
# чтобы он использовал указанные нами цвета.
# Дополните предложенный код:
#   
# library("ggplot2")
# mydata <- as.data.frame(HairEyeColor)
# obj <- ggplot(data = , aes(x = , y = Freq)) + 
#   geom_bar(stat="identity", position = ) + 
#   scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
# 
# У себя на компьютере вы можете визуализировать полученный график, исполнив 
# 'obj'. В случае, если все сделано правильно, он будет выглядеть так 
# (обратите внимание на название осей и легенды):
#   
# Прежде чем отправить код на проверку, выполните его на своем компьютере, 
# чтобы избежать лишних ошибок.
# При ошибке, обратите внимание на содержание feedback.

library("ggplot2")

mydata <- as.data.frame(HairEyeColor)

obj <- ggplot(data = mydata[mydata$Sex == 'Female',], aes(x = Hair, y = Freq, fill = Eye)) +
  geom_bar(stat="identity", position = position_dodge()) +
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))





##########################

# Binomial Test
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)


# Chi-Square
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp
chi$obs


t2
chisq.test(t2)



# Fisher's Exact Test

fisher.test(t2)

# На основе таблицы HairEyeColor создайте ещё одну таблицу, в которой хранится 
# информация о распределении цвета глаз у женщин-шатенок (Hair = 'Brown'). 
# Проведите тест равномерности распределения цвета глаз у шатенок и выведите 
# значение хи-квадрата для этого теста.
t1 <- HairEyeColor['Brown',,'Female']
chisq.test(t1)

# Воспользуемся данными diamonds из библиотеки ggplot2. При помощи критерия 
# Хи - квадрат проверьте гипотезу о взаимосвязи качества огранки бриллианта 
# (сut) и его цвета (color). В переменную main_stat сохраните значение 
# статистики критерия Хи - квадрат. Обратите внимание, main_stat должен быть 
#вектором из одного элемента, а не списком (листом).
# 
# Данные diamonds уже доступны для работы!

main_stat <- chisq.test(x = diamonds$cut, y = diamonds$color)$statistic


# Опять воспользуемся данными diamonds из библиотеки ggplot2. При помощи 
# критерия Хи - квадрат проверьте гипотезу о взаимосвязи цены (price) и 
# каратов (carat) бриллиантов. Для этого сначала нужно перевести эти 
# количественные переменные в формат пригодный для Хи - квадрат. Создайте две 
# новые переменные в данных diamonds:
#   
# factor_price - где будет 1, если значение цены больше либо равно чем среднее, 
# и 0, если значение цены ниже среднего цены по выборке.
# 
# factor_carat - где будет 1, если число карат больше либо равно чем среднее,  
# и 0, если ниже среднего числа карат по выборке.
# 
# Важный момент - на больших данных цикл for() работает довольно медленно, 
# постарайтесь решить эту задачу без его использования!
#   
# Используя эти шкалы при помощи Хи - квадрат проверьте исходную гипотезу. 
# Сохраните в переменную main_stat значение критерия  Хи - квадрат.
# 
# Пример перевода количественной шкалы в номинативную:
#   
# > x <- (1, 2, 3, 5, 6, 7) # mean(x) = 4
# > factor_x <- (0, 0, 0, 1, 1, 1)

diamonds$factor_price <- ifelse(diamonds$price >= mean(diamonds$price),1,0) 
diamonds$factor_carat <- ifelse(diamonds$carat >= mean(diamonds$carat),1,0) 
main_stat <- chisq.test(x = diamonds$factor_price, y = diamonds$factor_carat)$statistic


# При помощи точного критерия Фишера проверьте гипотезу о взаимосвязи типа 
# коробки передач (am) и типа двигателя (vs) в данных mtcars. Результат 
# выполнения критерия сохраните в переменную.Получившийся p - уровень значимости 
# сохраните в переменную fisher_test.

result <- fisher.test(x = mtcars$am, y = mtcars$vs)
fisher_test <- result$p.value





