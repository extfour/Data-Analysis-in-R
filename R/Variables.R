#Step 2: Variable assiment

# "=" так же работает как присваивание, но 
# принято использовать "<-"
my_var1  <- 42   
my_var2  <- 35.25

my_var1 + 100
my_var1 + my_var2 - 12
my_var3  <- my_var1^2 + my_var2^2

#Step 3: Logical opperartions
my_var3 > 200
my_var3 > 3009
my_var1 == my_var2
my_var1 != my_var2
my_var3 >= 200
my_var3 <= 200

my_new_var  <- my_var1 == my_var2

my_number <- 42
my_logical_var <- TRUE

# В уже созданной переменной var_1, в хранится целое число. 
# Создайте новую переменную var_2, которая равняется  var_1 умноженная на 10.
var_2 <- var_1 * 10


# В уже созданных переменных number_1, number_2 и number_3, 
# сохранены целые числа. Проверьте, действительно ли сумма 
# первых двух чисел строго больше, чем третье число. 
# Результат сравнения (TRUE или FALSE) сохраните в новую переменную 
# с именем result.
result <- number_1 + number_2 > number_3



#Step 6, 7, 10, 11: Vectors
1 : 67
my_vector1  <- 1:67
my_vector2  <- c(-32, 45, 67, 12.78, 129, 0, -65)

my_vector1[1]
my_vector1[3]

my_vector2[2]

my_vector2[c(1,2,3)]
my_vector2[c(3,4,1)]
my_vector2[1:3]
my_vector2[c(1,5,6,7,10)]

the_best_vector <- c(1:5000, 7000:10000)

my_numbers <- 1:21
my_numbers_2 <- my_numbers[c(2, 5, 7, 9, 12, 16, 20)]


my_vector1 + 10
my_vector2 + 56

my_vector2 == 0
my_vector1 > 30

x  <- 23
my_vector1 > 23
x == 23

my_vector2 > 0
my_vector2[my_vector2 > 0]
my_vector2[my_vector2 < 0]
my_vector2[my_vector2 == 0]

my_vector1[my_vector1 > 20 & my_vector1 < 30]
my_numbers  <- my_vector1[my_vector1 > 20 & my_vector1 < 30]
positive_numbers  <- my_vector2[my_vector2 > 0]


v1  <- c(165, 178, 180, 181, 167, 178, 187, 167, 187)
mean_v1  <- mean(v1)
v1[v1 > mean_v1]
greater_than_mean  <- v1[v1 > mean_v1]

# В уже созданной переменной my_vector хранится вектор из 20 целых чисел.
# Найдите сумму всех элементов вектора , которые больше 10. Сохраните 
# сумму в переменную my_sum.
my_sum <- sum(my_vector[my_vector > 10])

#Step 13: Lists and dataframes
age  <- c(16, 18, 22, 27)
is_maried  <- c(F, F, T, T)
name  <- c("Olga", "Maria", "Nastya", "Polina")
my_list <- list(age, name)
my_data  <- data.frame(Name = name, Age = age, Status = is_maried)


# В векторе  my_vector отберите только те наблюдения, 
# которые отклоняются от среднего меньше чем на одно 
# стандартное отклонение. Сохраните эти наблюдения в новую 
# переменную my_vector_2. 
# При этом исходный вектор my_vector оставьте без изменений.

# mean(x) среднее значение вектора x
# sd(x)  стандартное отклонение вектора x
# 
# также может пригодиться:
# abs(n)  абсолютное значение числа n

my_vector_2 <- my_vector[abs(my_vector - mean(my_vector)) < sd(my_vector)]
