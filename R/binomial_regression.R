library(ggplot2)

my_df <- read.csv("train.csv", sep=";")
str(my_df)

ggplot(my_df, aes(read, math, col = gender))+
  geom_point(size = 5)+
  facet_grid(.~hon)+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))


fit  <- glm(hon ~ read + math + gender, my_df, family = "binomial")
summary(fit)

exp(fit$coefficients)

head(predict(object = fit))

head(predict(object = fit, type = "response"))

my_df$prob  <- predict(object = fit, type = "response")


# Используем данные mtcars. Сохраните в переменную логистическую регрессионную
# модель, где в качестве зависимой переменной выступает тип коробки передач (am),
# в качестве предикторов переменные disp, vs, mpg.
# 
# Значения коэффициентов регрессии сохраните в переменную log_coef.

fit <- glm(am ~ disp + vs + mpg, mtcars, family = "binomial")
log_coef <- fit$coefficients


# Дополните предложенный в задании код, чтобы построить следующий график по
# данным ToothGrowth.
# Изобразите различия длины зубов морских свинок в различных условиях дозировки
# и типа потребляемого продукта.
# 
# По оси x - переменная supp.
# По оси y - переменная len.
# Цвет ящиков с усами (boxplot) - переменная dose.


library("ggplot2")

obj <- ggplot(data = ToothGrowth, aes(supp, len, fill=factor(dose)))+
  geom_boxplot()


library(ROCR)

pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc")
str(auc)



perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)

legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v= 0.225, lwd = 2)


my_df$pred_resp  <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))

my_df$correct  <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)


ggplot(my_df, aes(prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))
  
mean(my_df$correct)


test_df  <- read.csv("test.csv", sep = ";")
test_df$hon  <- NA

test_df$hon  <- predict(fit, newdata = test_df, type = "response")
View(test_df)


# Используем модельные данные о соотношении среднего и высшего образования в
# американских школах. Данные доступны по ссылке: 
#   https://stepik.org/media/attachments/lesson/11478/data.csv 
# Про часть испытуемых известно, поступили они в университет или нет 
# (переменная admit, 1 = поступили, 0 = не поступили), про остальных таких 
# данных нет (NA). Описание данных (обратите на него внимание при проведении 
#                                   подсчётов):
#   
# 'data.frame':  400 obs. of  4 variables:
# $ admit: Factor w/ 2 levels "0","1": 1 2 NA NA 1 2 NA NA 2 1 ...
# $ gre  : int  380 660 800 640 520 760 560 400 540 700 ...
# $ gpa  : num  3.61 3.67 4 3.19 2.93 3 2.98 3.08 3.39 3.92 ...
# $ rank : Factor w/ 4 levels "1","2","3","4": 3 3 1 4 4 2 1 2 3 2 ...
# 
# По имеющимся данным в переменной admit постройте логистическую регрессионную 
# модель, предсказывающую результат поступления по престижности учебного 
# заведения среднего образования 
# (переменная rank, 1 — наиболее престижное, 4 — наименее престижное) 
# и результатов GPA (переменная gpa) с учётом их взаимодействия. 
# Примените эту модель к той части данных, где результат поступления неизвестен.
# 
# Ответом в задаче будет предсказанное моделью число поступивших из тех, 
# для кого результат поступления был неизвестен. Считаем человека поступившим, 
# когда вероятность его поступления не меньше 0.4.

install.packages('ROCR')
library(ROCR)

df <- read.csv('data/data.csv')

df$rank <- factor(df$rank)
df$admit <- factor(df$admit)

fit <- glm(admit ~ gpa * rank, df, family = 'binomial', na.action = na.exclude)
pred_fit <- predict(fit, newdata=df[is.na(df$admit),], type = "response")

length(pred_fit[pred_fit>=0.4])
