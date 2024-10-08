#Step 1: Base graphs

df  <- mtcars
df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))

hist(df$mpg, breaks = 20, xlab = "MPG", main ="Histogram of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

plot(density(df$mpg), xlab = "MPG", main ="Density of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

boxplot(mpg ~ am, df, ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)

boxplot(df$mpg[df$am == "Auto"], df$mpg[df$am == "Manual"], ylab = "MPG", 
        main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)


plot(df$mpg, df$hp, xlab = "MPG", ylab ="HP" , main ="MPG and HP", pch = 22)

plot(~ mpg + hp, df) 


#Step 2, 3: Library ggplot2

library(ggplot2)

ggplot(df, aes(x = mpg))+
  geom_histogram(fill = "white", col = "black", binwidth = 2)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  ggtitle("MPG histogram")

ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot()+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG dotplot")


ggplot(df, aes(x = mpg))+
  geom_density(fill = "red")

ggplot(df, aes(x = mpg, fill = am))+
  geom_density(alpha = 0.5)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG density plot")


ggplot(df, aes(x = am, y = hp, fill = vs))+
  geom_boxplot()+
  xlab("Transmission type")+
  ylab("Gross horsepower")+
  scale_fill_discrete(name="Engine type")+
  ggtitle("Gross horsepower and engine type")


ggplot(df, aes(x = mpg, y = hp, size = qsec))+
  geom_point()+
  xlab("Miles/(US) gallon")+
  ylab("Gross horsepower")+
  scale_size_continuous(name="1/4 mile time")+
  ggtitle("Miles/(US) gallon and Gross horsepower")


my_plot  <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()

my_plot2  <- ggplot(df, aes(x = am, y = hp, fill = vs))

my_plot2 + geom_boxplot()


# При помощи функции ggplot() или boxplot() постройте график boxplot, используя 
# встроенные в R данные airquality. По оси x отложите номер месяца, по оси y — 
# значения переменной Ozone.
# 
# На графике boxplot отдельными точками отображаются наблюдения, отклоняющиеся 
# от 1 или 3 квартиля больше чем на полтора межквартильных размаха. Сколько 
# таких наблюдений присутствует в сентябре (месяц №9)?
#   
# Обратите внимание, что для корректного отображения графика ggplot ожидает 
# факторную переменную по оси x.

df <- airquality

ggplot(df, aes(x = Month, y = Ozone, group = Month))+
  geom_boxplot()+
  xlab("Month")+
  ylab("Ozone")

# Используем знакомые нам данные mtcars. 
# 
# Нужно построить scatterplot с помощью ggplot из ggplot2, по оси x которого 
# будет mpg, по оси y - disp, а цветом отобразить переменную (hp).
# 
# Полученный график нужно сохранить в переменную plot1. Таким образом в ответе 
# должен быть скрипт:
#   
#   plot1 <- ggplot(data, aes())+
#   geom_****()

df <- mtcars
df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))

plot1 <- ggplot(df, aes(x = mpg, y = disp, colour = hp))+
  geom_point()