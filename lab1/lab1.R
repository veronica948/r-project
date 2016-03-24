#Ex 1 
college = read.csv("College.csv")
View(college)
fix(college)
rownames(college) = college[, 1]
fix(college)
college = college[, -1]
fix(college)
summary(college)
pairs(college[,1:10])
plot(college$Private, college$Outstate, xlab="Private", ylab="Outstate")

Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
View(college)
summary(college)
summary(college$Elite)
plot(college$Elite, college$Outstate,xlab="Elite", ylab="Outstate")
plot(college$Outstate,college$Elite, ,xlab="Outstate", ylab="Elite")
par(mfrow = c(2, 2))
hist(college$Outstate)
hist(college$Apps)
hist(college$Enroll)
hist(college$Top10perc)
par(mfrow = c(1,1))

#Exercise2

# + Сколько строк в данном множестве? Сколько столбцов? Что представляют собой строки и столбцы?

# + Постройте несколько точечных диаграмм для предикторов (переменных) из этого множества. Опишите полученные результаты.

#Связаны ли какие-либо предикторы с уровнем преступности? Если да, то укажите каким образом.

#Имеют ли какие-либо районы Бостона чрезмерно высокий уровень преступности? Уровень налогов? Отношение количества учеников к количеству учителей? Прокомментируйте размах каждого из предикторов.

# + Сколько районов Бостона граничат с рекой Чарльз?

# + Какое медианное значение для отношения количества учеников к количеству учителей среди районов множества данных?

# + Какой из районов Бостона имеет наименьшее медианное значение для стоимости собственного жилья? Каковы значения других предикторов для этого района и как эти значения соотносятся со значениями данных предикторов в целом? Прокомментируйте ваши выводы.

# + В каком количестве районов среднее число комнат в жилом помещении больше 7? Больше 8? Прокомментируйте районы, в которых среднее число комнат в жилом помещении больше 8.

#a
library(MASS)
?Boston
boston = Boston
print("Amount of rows: ")
nrow(boston)
print("Amount of columns: ")
ncol(boston)
View(boston)
summary(boston)
str(boston)

#b
plot(boston$zn, boston$indus)
#zn - доля жилой земли for lots over 25,000 sq.ft.
#indus - proportion of non-retail business acres per town (нерозничный бизнес).
plot(boston$rm, boston$age)
#rm - Среднее количество комнат в жилище.
#age - доля занятых владельцами единиц, построенных до 1940.
plot(boston$nox,boston$dis)
#nox - концентрация оксидов азота (частей на 10 миллионов).
#dis - взвешенное среднее расстояний до пяти центров занятости.

#c

pairs(boston[,1:5])
pairs(boston[,c(1,6,7,8,9,10)])
pairs(boston[,c(1,11,12,13,14)])

#d
high_crim = boston[boston$crim > 50,]
nrow(high_crim)
max(boston$crim)
median(boston$crim)
high_tax = boston[boston$tax > 500,]
nrow(high_tax)
max(boston$tax)
median(boston$tax)
high_ptratio = boston[boston$ptratio > 20,]
nrow(high_ptratio)
max(boston$ptratio)
median(boston$ptratio)

#e
amoutNearRiver = sum(Boston$chas)

print("районы Бостона, которые граничат с рекой Чарльз")
print(amoutNearRiver)

#f
print("Медиана количества учеников к количеству учителей")
median(Boston$ptratio)

#h
avgRoom7 = subset(boston, boston$rm >7)
nrow(avgRoom7)
View(avgRoom7)

avgRoom8 = boston[boston$rm > 8,]
nrow(avgRoom8)
View(avgRoom8)
summary(avgRoom8)

#g
print("Районы с наименьшей медианой стоимости жилья")
minMedv = subset(Boston, Boston$medv == min(Boston$medv))
View(minMedv)
print(mean(Boston$medv) - min(Boston$medv))

