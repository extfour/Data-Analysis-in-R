#Step 2: Data preprocessing

?mtcars

df  <- mtcars

str(df)

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))


  #Step 3: Descriptive statistics

median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean_disp  <- mean(df$disp)

mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == "V"])

sd(df$hp[df$cyl != 3 & df$am == "Auto"])

# Вновь вернемся к данным mtcars. Рассчитайте среднее значение времени разгона 
# (qsec) для автомобилей, число цилиндров (cyl) у которых не равняется 3 и 
# показатель количества миль на галлон топлива (mpg) больше 20.
# 
# Получившийся результат (среднее значение) сохраните в переменную result.
result <- mean(mtcars$qsec[mtcars$cyl != 3 & mtcars$mpg > 20])


#Step 5: Aggregation

?aggregate

mean_hp_vs  <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)

colnames(mean_hp_vs)  <- c("VS", "Mean HP")

aggregate(hp ~ vs, df, mean)

aggregate(hp ~ vs + am, df, mean)
aggregate(x = df$hp, by = list (df$vs, df$am), FUN = mean)

aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median)

aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd)

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

# При помощи функции aggregate рассчитайте стандартное отклонение переменной 
# hp (лошадиные силы) и переменной disp (вместимости двигателя)  у машин с 
# автоматической и ручной коробкой передач. 
# 
# Полученные результаты (результаты выполнения функции aggregate) сохраните в 
# переменную descriptions_stat.

descriptions_stat  <- aggregate(cbind(hp, disp) ~ am, df, sd)


#Step 8, 9: Library "psych"


library(psych)

?describe

describe(x = df)

descr  <- describe(x = df[,-c(8,9)])

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs)

descr2$V
descr2$S

descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1)

descr3  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, 
           fast = T)


#Step 10: NA values

sum(is.na(df))

df$mpg[1:10]  <- NA

mean(df$mpg, na.rm = T)

aggregate(mpg ~am, df, sd)

describe(na.rm = )

# Воспользуемся встроенными данными airquality. В новую переменную сохраните 
# subset исходных данных, оставив наблюдения только для месяцев 7, 8 и 9.
# 
# При помощи функции aggregate рассчитайте количество непропущенных наблюдений 
# по переменной Ozone в 7, 8 и 9 месяце. Для определения количества наблюдений
# используйте функцию length().
# 
# Результат выполнения функции aggregate сохраните в переменную result.
# 
# Подсказки:
#   
# 1. Не забудьте сделать subset, чтобы отобрать наблюдения только по нужным 
#    месяцам, вам может пригодиться следующая конструкция:
#   
#   > x <- 5
#   > x %in% c(3, 4, 5)
#   [1] TRUE
# 
# 2. Для подсчета числа непропущенных наблюдений воспользуйтесь записью с 
#    помощью формулы, при которой пропущенные значения не учитываются:
#    aggregate(y ~ x + z , data, FUN)

df <- subset(airquality, airquality$Month %in% c(7,8,9))
result <- aggregate(Ozone ~ Month, df, length)

# Примените функцию describeBy к количественным переменным данных airquality, 
# группируя наблюдения по переменной Month.  Чему равен коэффициент асимметрии 
# (skew) переменной Wind в восьмом месяце?
#   
# В графу с ответом требуется ввести только число. Десятичный разделитель - 
# запятая: например 12,6

df <- airquality
res <- describeBy(df$Wind, group = list(df$Month), mat = T)
res$skew[res$group1 == 8]

describe(iris, fast = T)

res <- describeBy(iris, group = list(iris$Species), mat = T)

# В переменной my_vector сохранен вектор с пропущенными значениями. 
# Вам нужно создать новый вектор fixed_vector, в котором все пропущенные 
# значения вектора my_vector будут заменены на среднее значение по имеющимся 
# наблюдениям.
# 
# При этом исходный вектор оставьте без изменений!
#   
# Напоминаю, переменная my_vector уже создана, сразу начинайте работать с ней. 
# Перед тем, как сдавать решение, вы можете потренироваться на различных 
# примерах.
# Ниже небольшой код, который может создать случайный вектор (выборка из 
# нормального распределения) с пропущенными значениями.
# 
# my_vector <- rnorm(30)
# my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA
# 
# Задача для самостоятельной работы:
#   
#   Изучите справку по функции replace. Вызвать справку можно исполнив команду:
#   
#   ?replace
# Попробуйте решить это задание при помощи этой функции.

my_vector <- rnorm(30)
my_vector[sample(1:30, 10)] <- NA

mean_v <-  mean(my_vector,na.rm = T)
fixed_vector <- replace(my_vector, is.na(my_vector), mean_v)
