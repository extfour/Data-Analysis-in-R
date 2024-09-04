my_calc  <- function(x, y){
  s  <- x + y
  return(s)
}

my_calc(x = 10, y = 15)
result  <- my_calc(10, 15)

my_calc_2  <- function(x, y){
  s  <- x + y
  d  <- x - y
  return(c(s, d))
}

my_calc_2(10, 15)


my_calc_3  <- function(x, y, z){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3  <- function(x, y, z = 10){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}



############

distr1  <- rnorm(100)
distr1[1:30]  <- NA
distr1[is.na(distr1)]  <- mean(distr1, na.rm = T)

my_na_rm  <- function(x){
  if (is.numeric(x)){
    stat_test  <- shapiro.test(x)
    if (stat_test$p.value > 0.05){
      x[is.na(x)]  <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)]  <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    print("X is not numeric")
  }
}

d1  <- rnorm(2000)
d2  <- runif(2000)

d1[1:10]  <- NA
d2[1:10]  <- NA

d1  <- my_na_rm(d1)
head(d1)

d2  <- my_na_rm(d2)
head(d2)


######################

read_data  <- function(){
  df  <- data.frame()
  number  <<- 0
  for (i in dir(pattern = "*.csv")){
    temp_df  <- read.csv(i, stringsAsFactors = F)
    df  <- rbind(temp_df, df)
    number <<- number + 1
    }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

# Напишите функцию, которая выводит номера позиций пропущенных наблюдений в векторе.
# 
# На вход функция получает числовой вектор с пропущенными значениями. Функция возвращает
# новый вектор с номерами позиций пропущенных значений.
# 
# Подсказка: чтобы проверить является ли наблюдение NA, воспользуйтесь функцией is.na(), 
# кстати, функция векторизирована, и аргументом может служить вектор произвольной длинны. 
# Запись x == NA ни к чему осмысленному не приведет. Т.к. если x это NA, то команда x == NA
# также вернет NA, а не TRUE!
#   
# > my_vector <- c(1, 2, 3, NA, NA)
# > NA.position(my_vector)
# [1] 4 5

my_na_rm  <- function(x){
  i <- 1
  indexs <- c()
  for (variable in x) {
    if (is.na(variable)) {
      indexs <- c(indexs, i)
    }
    i <- i + 1
  }
  return(indexs)
}
my_na_rm(c(1, 2, 3, NA, NA))

# Напишите функцию NA.counter для подсчета пропущенных значений в векторе.
# 
# На вход функция  NA.counter должна принимать один аргумент - числовой вектор. 
# Функция должна возвращать количество пропущенных значений.
# 
# > my_vector <- c(1, 2, 3, NA, NA)
# > NA.counter(my_vector)
# [1] 2 

NA.counter  <- function(x){
  length(which(is.na(x)))
}



# Advanced method without for loop

read_data_advanced <- function(){
    df <- do.call(rbind, lapply(dir(pattern = "*.csv"), 
                                read.csv, stringsAsFactors = F))
    return(df)
}

df  <- data.frame(x = factor(1:5))
df1  <- data.frame(x = factor(7:8))
str(df)
str(df1)

df3  <- rbind(df, df1)
str(df3)
table(df3$x)


 
# Напишите функцию filtered.sum, которая на вход получает вектор с пропущенными, 
# положительными и отрицательными значениями и возвращает сумму положительных элементов 
# вектора.
# 
# >  filtered.sum(c(1, -2, 3, NA, NA))
# [1] 4 


filtered.sum <- function(x){
    sum(x[!is.na(x) & x > 0])
}
filtered.sum(c(1, -2, 3, NA, NA))

# Задача для героев!
#   
# Напишите функцию outliers.rm, которая находит и удаляет выбросы. Для обнаружения выбросов
# воспользуемся самым простым способом, с которым вы не раз встречались, используя график 
# Box plot. 
# 
# Выбросами будем считать те наблюдения, которые отклоняются от 1 или 3 квартиля больше чем
# на 1,5 *  IQR, где  IQR  - межквартильный размах.
# 
# На вход функция получает числовой вектор x. Функция должна возвращать модифицированный
# вектор x с удаленными выбросами. 
# 
# Ссылка на видео с объяснением, как на графике box-plot отображаются выбросы:
#   
# https://stepik.org/lesson/%D0%9A%D0%B2%D0%B0%D1%80%D1%82%D0%B8%D0%BB%D0%B8-%D1%80%D0%B0%D1%81%D0%BF%...
# 
# Полезные функции:
#   
#   IQR(x) - рассчитывает межквартильный размах вектора x
# 
# quantile(x, probs = c(0.25, 0.75)) - рассчитывает первый и третий квартиль вектора x   
x <- c(1, 10,11,12,13,15,15,17,18,20,100)

outliers.rm <- function(vec) {
  quant <- quantile(vec, probs = c(0.25, 0.75))
  
  vec <- vec[!(vec < unname(quant)[1] - 1.5 * IQR(vec) | vec > unname(quant)[2] + 1.5 * IQR(vec))]
  
  return(vec)
}
outliers.rm(x)

boxplot(x)

