#Задание 1

my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 1500)
my.dataframe <- data.frame(my.file)
str(my.dataframe)

#Функция density() используется для отображения (ядерной) оценки плотности
#Визуализацию реализуем с помощью hist(), plot() и lines()
#Рисуем гистограмму
hist(my.dataframe$Total.day.minutes, breaks = 25, freq = FALSE, col = "thistle", 
     main = paste("Количество минут использования днём (частота)"), 
     xlab = "Количество днём (минуты)")
#Добавляем линии ядерной оценки плотности
lines(density(my.dataframe$Total.day.minutes), col = "seagreen", lwd = 1.5)
lines(density(my.dataframe$Total.day.minutes, bw = 0.85), col = "slateblue", lwd = 1.5)

#
#Установка пакета sm ("smoothing methods"), если его нет в системе
#install.packages("sm")
library(sm)

#Сравнение данных из разных штатов по кривым ядерной плотности 
#Для того, чтобы сделать легенду на графике, необходим вектор factor
Day.minutes <- data.frame(as.factor(my.dataframe$State), my.dataframe$Total.day.minutes, my.dataframe$State)

Colfill <- c(20:(3 + length(levels(Day.minutes$as.factor.my.dataframe.State.))))
sm.density.compare(Day.minutes$my.dataframe.Total.day.minutes, Day.minutes$as.factor.my.dataframe.State., lwd = 2, col = Colfill, method = "sj",
                   xlab = "Количество днём (минуты)")
title(main = "Кривые плотности по количеству минут днём в разных штатах")
legend(locator(1), legend = levels(Day.minutes$as.factor.my.dataframe.State.), fill = Colfill, title = "Site")


#Иллюстрация двумерной ядерной оценки плотности
library(MASS)
#Двумерная ядерная оценка для совместного распределения 
f <- kde2d(my.dataframe$Total.day.calls, my.dataframe$Total.eve.calls)
#Создание окрашенной прямоугольной сетки
image(f, xlab="Количество звонков днём", ylab="Количество звонков вечером", ylim= range(40, 140))
#Добавляем на график изолинии
contour(f, add = TRUE)


#Задание 2
#cdplot ("conditional density plot")
#Вывод на одном графике плотности вероятности для каждого уровня качественной переменной

Day.calls.churn <- data.frame(as.factor(my.dataframe$Churn), my.dataframe$Total.day.calls)
#Вывод нескольких графиков на одном листе
layout(matrix(1:3, ncol = 3)) 

#Рассматриваем количество звонков днём у пользователей с прдлённой или отменённой подпиской 
#с различными параметрами сглаживания
cdplot(Day.calls.churn$as.factor.my.dataframe.Churn. ~ Day.calls.churn$my.dataframe.Total.day.calls, 
       col = c("olivedrab", "peachpuff"), 
       xlab = "Количество звонков днём", 
       ylab = "Наличие подписки",
       xlim = range(0, 150))

cdplot(Day.calls.churn$as.factor.my.dataframe.Churn. ~ Day.calls.churn$my.dataframe.Total.day.calls, 
       col = c("olivedrab", "peachpuff"), 
       xlab = "Количество звонков днём", 
       ylab = "Наличие подписки",
       xlim = range(0, 150), bw = 0.95)

cdplot(Day.calls.churn$as.factor.my.dataframe.Churn. ~ Day.calls.churn$my.dataframe.Total.day.calls, 
       col = c("olivedrab", "peachpuff"), 
       xlab = "Количество звонков днём", 
       ylab = "Наличие подписки",
       xlim = range(0, 150), bw = 0.9)

#dotchart Точечные диаграммы Кливленда
#Возьмём первые 10 записей из таблицы и посмотрим сколько всего двонков (днём и вечером)
#совершали эти пользователи, как подписи используем штаты этих пользователей
layout(matrix(1:1, ncol = 1)) 
for.chart <- my.dataframe[c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), ]
dotchart((for.chart$Total.day.calls + for.chart$Total.eve.calls), labels = for.chart$State,
         main="Количество звонков в день у различных пользователей",
         xlab="количество звонков в день", cex = 0.7, pch = 8,  color = "steelblue", lcolor = "gray")


#Примеры boxplot (диаграммы размахов / "ящики с усами")
#Точки или линию, соответствующую некоторой мере центральной тенденции в данных,
#окружает прямоугольник ("ящик"), длина которого соответствует определенному
#показателю разброса. Дополнительно от этого прямоугольника отходят "усы",
#также отражающие разброс в данных или, реже, точность оценки меры центральной тенденции
#+ разница между двумя и более группами

par(mfrow = c(1, 2))
#Слева от знака ~ указывается зависимая переменная, справа – предикторы
boxplot(Day.calls.churn$my.dataframe.Total.day.calls ~ Day.calls.churn$as.factor.my.dataframe.Churn.,
        xlab = "Наличие подписки",
        ylab = "Количество звонков днём",
        main = "Количество звонков днём у пользователей с и без подписки",
        col = "thistle2")

# Горизонтальное расположение "ящиков"
boxplot(Day.calls.churn$my.dataframe.Total.day.calls ~ Day.calls.churn$as.factor.my.dataframe.Churn.,
        ylab = "Наличие подписки",
        xlab = "Количество звонков днём",
        horizontal = TRUE,
        main = "Количество звонков днём у пользователей с и без подписки",
        col = "thistle2")


#stripchart Одномерные диаграммы рассеяния
stripchart(Day.calls.churn$my.dataframe.Total.day.calls ~ Day.calls.churn$as.factor.my.dataframe.Churn.,
           main = "Количество звонков дём у пользователей с и без подписки",
           xlab = "Наличие подписки",
           ylab = "Количество звонков",
           vertical = TRUE,
           method = "jitter",
           jitter = 0.01,
           pch = 20, col = "steelblue")

#Совместим stripchart с boxplot
boxplot(Day.calls.churn$my.dataframe.Total.day.calls ~ Day.calls.churn$as.factor.my.dataframe.Churn.,
        xlab = "Наличие подписки",
        ylab = "Количество звонков днём",
        main = "Количество звонков днём у пользователей с и без подписки",
        col = "thistle2")
stripchart(Day.calls.churn$my.dataframe.Total.day.calls ~ Day.calls.churn$as.factor.my.dataframe.Churn., 
           method="jitter", 
           data = Day.calls.churn, add = TRUE,
           pch = 1, lwd = 1.7, col = "plum", vertical = TRUE)


#Задание 3

library(outliers)

hist(my.dataframe$Total.day.minutes, breaks = 10, freq = FALSE, col = "thistle", 
     main = paste("Количество минут использования днём (частота)"), 
     xlab = "Количество днём (минуты)")

#Проверка нормальности выборки
criteria <- my.dataframe[c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20), ]
(dofx <- fitdistr(criteria$Total.day.minutes,"normal"))
ep1x <- dofx$estimate[1]; ep2x <- dofx$estimate[2]
ks.test(criteria$Total.day.minutes, pnorm, mean = ep1x, sd = ep2x)

#Построим boxplot и stripchart для визуализации выбросов
boxplot(criteria$Total.day.minutes ~ criteria$Churn,
        xlab = "Наличие подписки",
        ylab = "Количество звонков днём",
        main = "Количество звонков днём у пользователей с и без подписки",
        col = "thistle2")
stripchart(criteria$Total.day.minutes ~ criteria$Churn, 
           method="jitter", 
           data = Day.calls.churn, add = TRUE,
           pch = 1, lwd = 1.7, col = "plum", vertical = TRUE)

#Проверим на выбросы критерием Граббса
grubbs.test(criteria$Total.day.minutes)
grubbs.test(criteria$Total.day.minutes, opposite = TRUE)
grubbs.test(criteria$Total.day.minutes, two.sided = TRUE)
grubbs.test(criteria$Total.day.minutes, type = 20)
grubbs.test(criteria$Total.day.minutes, type = 11)

#Q-тест Диксона
dixon.test(criteria$Total.day.minutes)
dixon.test(criteria$Total.day.minutes, two.sided = TRUE)
dixon.test(criteria$Total.day.minutes, opposite = TRUE)
dixon.test(criteria$Total.day.minutes, type = 22)#type = 22потому, что больше 14 значений

#Задание 4

library(mice)

test <- my.dataframe[c(0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60,
                       64, 68, 72, 76, 80, 84, 88, 92, 96, 100, 104, 108, 112, 116), ]
summary(test)
test
#Создадим пропуски
test[c(2, 6, 10, 21), 8] <- NA
test[c(7, 17, 27), 11] <- NA
test[c(11, 18, 25), 13] <- NA
test

test2 <- mice(test)
test2 <- complete(test2)
test2


#Задание 5

library(MASS)

#Генерируем малые выборки x, y, z
set.seed(202)
x = sort(rnorm(100, 15, 2))

set.seed(115)
y = sort(rnorm(100, 3, 1))

set.seed(50)
z = sort(rnorm(100, 60, 0.5))

#Визуализация x, y, z 
par(mfrow = c(1,3))
hist(x, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность")
lines(density(x), lwd = 2, col = "violetred4")

hist(y, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность")
lines(density(y), lwd = 2, col = "violetred4")

hist(z, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность")
lines(density(z), lwd = 2, col = "violetred4")

#Проверка нормальности выборок
(dofx <- fitdistr(x,"normal"))
ep1x <- dofx$estimate[1]; ep2x <- dofx$estimate[2]

(dofy <- fitdistr(y,"normal"))
ep1y <- dofy$estimate[1]; ep2y <- dofy$estimate[2]

(dofz <- fitdistr(z,"normal"))
ep1z <- dofz$estimate[1]; ep2z <- dofz$estimate[2]

library(nortest)

ks.test(x, pnorm, mean = ep1x, sd = ep2x)
ks.test(y, pnorm, mean = ep1y, sd = ep2y)
ks.test(z, pnorm, mean = ep1z, sd = ep2z)

shapiro.test(x)
shapiro.test(y)
shapiro.test(z)

ad.test(x)
ad.test(y)
ad.test(z)

cvm.test(x)
cvm.test(y)
cvm.test(z)

lillie.test(x)
lillie.test(y)
lillie.test(z)

sf.test(x)
sf.test(y)
sf.test(z)

# Функция для вывода графиков теоретической и эмпирической кумулятивной функций распределения
graph_distr <- function(x, pc, pd, main_name = "")
{ 
  op <- par(mfrow = c(1, 1), pty = "s")
  par(mfrow = c(1, 2))
  mn <- paste(c("Эмпирическая ФР и ", main_name))
  plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
  mn <- paste(c("Эмпирическая плотность и ", main_name))
  plot(density(x), lwd = 2, col = "tan", main = mn) 
  lines(x, pd, col = "olivedrab", lwd = 2)
  par(op)
}

#Графики эмпирических функций распределения
graph_distr(x, pnorm(x, mean = ep1x, sd = ep2x),
            dnorm(x, mean = ep1x, sd = ep2x),
            "нормальное распределение x")
graph_distr(y, pnorm(y, mean = ep1y, sd = ep2y),
            dnorm(y, mean = ep1y, sd = ep2y),
            "нормальное распределение y")
graph_distr(z, pnorm(z, mean = ep1z, sd = ep2z),
            dnorm(z, mean = ep1z, sd = ep2z),
            "нормальное распределение z")

#Квантили
par(mfrow = c(1,3))
qqnorm(x, main = "Normal Q-Q Plot X")
qqline(x)
qqnorm(y, main = "Normal Q-Q Plot Y")
qqline(y)
qqnorm(z, main = "Normal Q-Q Plot Z")
qqline(z)

#Метод огибающих
library(car)
par(mfrow = c(1, 3))
qqPlot(x, dist = "norm", col = "black", col.lines = "olivedrab", pch = 10,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для X")
qqPlot(y, dist = "norm", col = "black", col.lines = "olivedrab", pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для Y")
qqPlot(z, dist = "norm", col = "black", col.lines = "olivedrab", pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для Z")

library(sm)
sm.density(x, model = "Normal", xlab = "Имитированная выборка",
           ylab = "Функция плотности распределения X", col.band = "olivedrab")
sm.density(y, model = "Normal", xlab = "Имитированная выборка",
           ylab = "Функция плотности распределения Y", col.band = "olivedrab")
sm.density(z, model = "Normal", xlab = "Имитированная выборка",
           ylab = "Функция плотности распределения Z", col.band = "olivedrab")

#То же самое для выборок умеренного объёма

#Генерируем выборки x, y, z
set.seed(2020)
x = sort(rnorm(2000, 1500, 2))

set.seed(1500)
y = sort(rnorm(2000, 713, 10))

set.seed(1000)
z = sort(rnorm(2000, 50, 0.5))

#Визуализируем x, y, z 
par(mfrow = c(1,3))
hist(x, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность")
lines(density(x), lwd = 2, col = "violetred4")

hist(y, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность")
lines(density(y), lwd = 2, col = "violetred4")

hist(z, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность")
lines(density(z), lwd = 2, col = "violetred4")

#Проверка нормальности выборок
(dofx <- fitdistr(x,"normal"))
ep1x <- dofx$estimate[1]; ep2x <- dofx$estimate[2]

(dofy <- fitdistr(y,"normal"))
ep1y <- dofy$estimate[1]; ep2y <- dofy$estimate[2]

(dofz <- fitdistr(z,"normal"))
ep1z <- dofz$estimate[1]; ep2z <- dofz$estimate[2]

library(nortest)

ks.test(x, pnorm, mean = ep1x, sd = ep2x)
ks.test(y, pnorm, mean = ep1y, sd = ep2y)
ks.test(z, pnorm, mean = ep1z, sd = ep2z)

shapiro.test(x)
shapiro.test(y)
shapiro.test(z)

ad.test(x)
ad.test(y)
ad.test(z)

cvm.test(x)
cvm.test(y)
cvm.test(z)

lillie.test(x)
lillie.test(y)
lillie.test(z)

sf.test(x)
sf.test(y)
sf.test(z)

# Функция для вывода графиков теоретической и эмпирической кумулятивной функций распределения
graph_distr <- function(x, pc, pd, main_name = "")
{ 
  op <- par(mfrow = c(1, 1), pty = "s")
  par(mfrow = c(1, 2))
  mn <- paste(c("Эмпирическая ФР и ", main_name))
  plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
  mn <- paste(c("Эмпирическая плотность и ", main_name))
  plot(density(x), lwd = 2, col = "tan", main = mn) 
  lines(x, pd, col = "olivedrab", lwd = 2)
  par(op)
}

#Графики эмпирических функций распределения
graph_distr(x, pnorm(x, mean = ep1x, sd = ep2x),
            dnorm(x, mean = ep1x, sd = ep2x),
            "нормальное распределение x")
graph_distr(y, pnorm(y, mean = ep1y, sd = ep2y),
            dnorm(y, mean = ep1y, sd = ep2y),
            "нормальное распределение y")
graph_distr(z, pnorm(z, mean = ep1z, sd = ep2z),
            dnorm(z, mean = ep1z, sd = ep2z),
            "нормальное распределение z")

#Квантили
par(mfrow = c(1,3))
qqnorm(x, main = "Normal Q-Q Plot X")
qqline(x)
qqnorm(y, main = "Normal Q-Q Plot Y")
qqline(y)
qqnorm(z, main = "Normal Q-Q Plot Z")
qqline(z)

#Метод огибающих
library(car)
par(mfrow = c(1, 3))
qqPlot(x, dist = "norm", col = "black", col.lines = "olivedrab", pch = 10,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для X")
qqPlot(y, dist = "norm", col = "black", col.lines = "olivedrab", pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для Y")
qqPlot(z, dist = "norm", col = "black", col.lines = "olivedrab", pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для Z")

library(sm)
sm.density(x, model = "Normal", xlab = "Имитированная выборка",
           ylab = "Функция плотности распределения X", col.band = "olivedrab")
sm.density(y, model = "Normal", xlab = "Имитированная выборка",
           ylab = "Функция плотности распределения Y", col.band = "olivedrab")
sm.density(z, model = "Normal", xlab = "Имитированная выборка",
           ylab = "Функция плотности распределения Z", col.band = "olivedrab")


#Задание 6

library(MASS)

#Создаем выборки малого объёма

my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 100)
my.dataframe <- data.frame(my.file)

x <- my.dataframe$Total.day.minutes

y <- (my.dataframe$Total.day.minutes + my.dataframe$Total.eve.minutes)

#Визуализация x, y
par(mfrow = c(1,2))
hist(x, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность", xlab = "Количество минут днём")
lines(density(x), lwd = 2, col = "violetred4")

hist(y, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность", xlab = "Количество минут днем и вечером суммарно")
lines(density(y), lwd = 2, col = "violetred4")


#Проверка нормальности выборок
(dofx <- fitdistr(x,"normal"))
ep1x <- dofx$estimate[1]; ep2x <- dofx$estimate[2]

(dofy <- fitdistr(y,"normal"))
ep1y <- dofy$estimate[1]; ep2y <- dofy$estimate[2]


library(nortest)

ks.test(x, pnorm, mean = ep1x, sd = ep2x)
ks.test(y, pnorm, mean = ep1y, sd = ep2y)

shapiro.test(x)
shapiro.test(y)

ad.test(x)
ad.test(y)

cvm.test(x)
cvm.test(y)

lillie.test(x)
lillie.test(y)

sf.test(x)
sf.test(y)

# Функция для вывода графиков теоретической и эмпирической кумулятивной функций распределения
graph_distr <- function(x, pc, pd, main_name = "")
{ 
  op <- par(mfrow = c(1, 1), pty = "s")
  par(mfrow = c(1, 2))
  mn <- paste(c("Эмпирическая ФР и ", main_name))
  plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
  mn <- paste(c("Эмпирическая плотность и ", main_name))
  plot(density(x), lwd = 2, col = "tan", main = mn) 
  lines(x, pd, col = "olivedrab", lwd = 2)
  par(op)
}

#Графики эмпирических функций распределения
graph_distr(x, pnorm(x, mean = ep1x, sd = ep2x),
            dnorm(x, mean = ep1x, sd = ep2x),
            "нормальное распределение x")
graph_distr(y, pnorm(y, mean = ep1y, sd = ep2y),
            dnorm(y, mean = ep1y, sd = ep2y),
            "нормальное распределение y")

#Квантили
par(mfrow = c(1,2))
qqnorm(x, main = "Normal Q-Q Plot X")
qqline(x)
qqnorm(y, main = "Normal Q-Q Plot Y")
qqline(y)

#Метод огибающих
library(car)
par(mfrow = c(1, 2))
qqPlot(x, dist = "norm", col = "black", col.lines = "olivedrab", pch = 10,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для X")
qqPlot(y, dist = "norm", col = "black", col.lines = "olivedrab", pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для Y")

library(sm)
sm.density(x, model = "Normal", xlab = "Количество минут днём",
           ylab = "Функция плотности распределения X", col.band = "olivedrab")
sm.density(y, model = "Normal", xlab = "Количество минут днем и вечером суммарно",
           ylab = "Функция плотности распределения Y", col.band = "olivedrab")

#То же самое для выборок умеренного объёма

my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 1500)
my.dataframe <- data.frame(my.file)

x <- my.dataframe$Total.day.minutes

y <- (my.dataframe$Total.day.minutes + my.dataframe$Total.eve.minutes)

#Визуализация x, y
par(mfrow = c(1,2))
hist(x, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность", xlab = "Количество минут днём")
lines(density(x), lwd = 2, col = "violetred4")

hist(y, freq = FALSE, breaks = 15, col = "lavenderblush", 
     main="Гистограмма и ядерная плотность", xlab = "Количество минут днем и вечером суммарно")
lines(density(y), lwd = 2, col = "violetred4")


#Проверка нормальности выборок
(dofx <- fitdistr(x,"normal"))
ep1x <- dofx$estimate[1]; ep2x <- dofx$estimate[2]

(dofy <- fitdistr(y,"normal"))
ep1y <- dofy$estimate[1]; ep2y <- dofy$estimate[2]


library(nortest)

ks.test(x, pnorm, mean = ep1x, sd = ep2x)
ks.test(y, pnorm, mean = ep1y, sd = ep2y)

shapiro.test(x)
shapiro.test(y)

ad.test(x)
ad.test(y)

cvm.test(x)
cvm.test(y)

lillie.test(x)
lillie.test(y)

sf.test(x)
sf.test(y)

# Функция для вывода графиков теоретической и эмпирической кумулятивной функций распределения
graph_distr <- function(x, pc, pd, main_name = "")
{ 
  op <- par(mfrow = c(1, 1), pty = "s")
  par(mfrow = c(1, 2))
  mn <- paste(c("Эмпирическая ФР и ", main_name))
  plot(x,pc, type = "l", col = "red", lwd = 2, main = mn) 
  plot(ecdf(x), add = TRUE) 
  mn <- paste(c("Эмпирическая плотность и ", main_name))
  plot(density(x), lwd = 2, col = "tan", main = mn) 
  lines(x, pd, col = "olivedrab", lwd = 2)
  par(op)
}

#Графики эмпирических функций распределения
graph_distr(x, pnorm(x, mean = ep1x, sd = ep2x),
            dnorm(x, mean = ep1x, sd = ep2x),
            "нормальное распределение x")
graph_distr(y, pnorm(y, mean = ep1y, sd = ep2y),
            dnorm(y, mean = ep1y, sd = ep2y),
            "нормальное распределение y")

#Квантили
par(mfrow = c(1,2))
qqnorm(x, main = "Normal Q-Q Plot X")
qqline(x)
qqnorm(y, main = "Normal Q-Q Plot Y")
qqline(y)

#Метод огибающих
library(car)
par(mfrow = c(1, 2))
qqPlot(x, dist = "norm", col = "black", col.lines = "olivedrab", pch = 10,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для X")
qqPlot(y, dist = "norm", col = "black", col.lines = "olivedrab", pch = 19,
       xlab="Квантили нормального распределения", 
       ylab="Наблюдаемые квантили", 
       main="Сравнение квантилей ЭФР и НР для Y")
library(sm)
sm.density(x, model = "Normal", xlab = "Количество минут днём",
           ylab = "Функция плотности распределения X", col.band = "olivedrab")
sm.density(y, model = "Normal", xlab = "Количество минут днем и вечером суммарно",
           ylab = "Функция плотности распределения Y", col.band = "olivedrab")


#Задание 7
library(car)

#Минимальный объем выборки
power.t.test(delta = 18.0, sd = 10, sig.level = 0.05, power = 0.8)

#Расчет мощности критерия
power.t.test(n = 667, delta = 18.0, sd = 10, sig.level = 0.05)

#Необходимый размер выборок для «парного» критерия 
power.t.test(delta = 18.0, sd = 10, sig.level = 0.05, 
             power = 0.8, type  = "paired")

#Необходимый размер выборок для одновыборочного критерия 
power.t.test(delta = 18.0, sd = 10, sig.level = 0.05, 
             power = 0.8, type  = "one.sample")


#Выборки имеют равные дистперсии для for t.test, bartlett.test, var.test, leveneTest
set.seed(10)
a = sort(rnorm(10, 30, 1))
set.seed(10)
b = sort(rnorm(10, 32, 1))
par(mfrow = c(1,2))

k = c('a', 'b', 'a', 'a', 'a', 'b', 'a', 'b', 'b', 'b')

set_ab <- c(a, b)
group_ab <- as.factor(c(rep(1, length(a)), rep(2, length(b))))

#Для wilcox.test и flinger.test
set.seed(10)
f = sort(rt(10, 2))
set.seed(10)
g = sort(rt(10, 2))

#
f <- c(4.86, 4.57, 4.43, 4.35, 5.73, 5.37, 5.03, 5.44, 6.15, 7.39)
g <- c(4.80, 4.50, 4.43, 4.40, 5.83, 5.47, 5.13, 5.54, 6.25, 7.50)

set_fg <- c(f, g)
group_fg <- as.factor(c(rep(1, length(f)), rep(2, length(g))))

#t.test
tapply(a, b, mean)
t.test(a ~ k)
t.test(a, b, var.equal = TRUE, conf.level = 0.99)
t.test(a, b, var.equal = TRUE, conf.level = 0.95)
t.test(a, b, var.equal = TRUE, conf.level = 0.90)

# Минимальный объем выборки
power.t.test(delta = 2.0, sd = 1, sig.level = 0.05, power = 0.8)

# Расчет мощности критерия
power.t.test(n = 10, delta = 2.0, sd = 1, sig.level = 0.05)

# Необходимый размер выборок для «парного» критерия 
power.t.test(delta = 2.0, sd = 1, sig.level = 0.05, 
             power = 0.8, type  = "paired")

# Необходимый размер выборок для одновыборочного критерия 
power.t.test(delta = 2.0, sd = 1, sig.level = 0.05, 
             power = 0.8, type  = "one.sample")

t.test(a ~ k, alternative = "two.sided", var.equal = TRUE, conf.level = 0.99) 
t.test(a ~ k, alternative = "two.sided", var.equal = TRUE, conf.level = 0.95) 
t.test(a ~ k, alternative = "two.sided", var.equal = TRUE, conf.level = 0.90) 

t.test(a ~ k, alternative = "greater", var.equal = TRUE, conf.level = 0.99) 
t.test(a ~ k, alternative = "greater", var.equal = TRUE, conf.level = 0.95) 
t.test(a ~ k, alternative = "greater", var.equal = TRUE, conf.level = 0.90)

t.test(a ~ k, alternative = "less", var.equal = TRUE, conf.level = 0.99) 
t.test(a ~ k, alternative = "less", var.equal = TRUE, conf.level = 0.95) 
t.test(a ~ k, alternative = "less", var.equal = TRUE, conf.level = 0.90) 

#wilcox.test
wilcox.test(f, mu = 5)
wilcox.test(f ~ k, paired = FALSE)
wilcox.test(f, g, paired = TRUE, conf.int = FALSE, conf.level = 0.99)
wilcox.test(f, g, paired = TRUE, conf.int = FALSE, conf.level = 0.95)
wilcox.test(f, g, paired = TRUE, conf.int = FALSE, conf.level = 0.90)

#var.test
var.test(a ~ k, conf.level = 0.99)
var.test(a ~ k, conf.level = 0.95)
var.test(a ~ k, conf.level = 0.90)

#levene.test
set_ak <- data.frame(c(a, k))
leveneTest(set_ab, group_ab)
set_ab <- data.frame(set_ab)
leveneTest(a ~ k, data = set_ab)

#bartlett.test
bartlett.test(a ~ k, data = set_ak)

#fligner.test
fligner.test(a ~ k, data = set_ak)


#Задание 8

#Векторы для анализа
minutes <- my.dataframe$Total.day.minutes
charge <- my.dataframe$Total.day.charge
night.minutes <- my.dataframe$Total.eve.minutes

shapiro.test(minutes)
shapiro.test(charge)
shapiro.test(night.minutes)

#Коэффициент корреляции Пирсона
cor.test(minutes, night.minutes)
cor.test(minutes, night.minutes, alternative = "greater")
cor.test(minutes, night.minutes, alternative = "less")

cor.test(minutes, charge)
cor.test(minutes, charge, alternative = "greater")
cor.test(minutes, charge, alternative = "less")

#Коэффициент корреляции Спирмана
cor.test(minutes, charge, method = "spearman")
cor.test(minutes, charge, method = "spearman", alternative = "greater")
cor.test(minutes, charge, method = "spearman", alternative = "less")

#Коэффициент корреляции Кендалла
cor.test(minutes, charge, method = "kendall")
cor.test(minutes, charge, method = "kendall", alternative = "greater")
cor.test(minutes, charge, method = "kendall", alternative = "less")

day <- data.frame(my.dataframe$Area.code, my.dataframe$Total.day.calls, my.dataframe$Total.day.charge, my.dataframe$Total.day.minutes)
day$my.dataframe.Area.code <- factor(day$my.dataframe.Area.code)
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(n = 11, name = 'Set3')
colors <- brewer.pal(n = 11, name = 'Set3')[day$my.dataframe.Area.code]

#Коэффициент корреляции Спирмана (матрица)
pairs(day, panel = panel.smooth)
panel.cor.s <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "spearman"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if(missing(cex.cor))
    cex.cor <- 1.1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(day, panel = panel.smooth, lower.panel = panel.cor.s, col = colors)

#Коэффициент корреляции Кендалла (матрица)
panel.cor.k <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, method = "kendall"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if(missing(cex.cor))
    cex.cor <- 1.1/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(day, panel = panel.smooth, lower.panel = panel.cor.k, col = colors)

#Matrix scattering diagoams
library(lattice)
splom(day, col = colors)
library(GGally)
ggpairs(day, 
        upper = list(continuous = "density", combo = "box"), 
        lower = list(continuous = "points", combo = "dot"))
#EDF
library(mgcv)
summary(gam(day$my.dataframe.Total.day.charge ~ s(day$my.dataframe.Total.day.calls) + 
              s(day$my.dataframe.Total.day.minutes)))


#Задание 9

#Векторы для анализа
my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 100)
my.dataframe <- data.frame(my.file)

table <- data.frame(my.dataframe$Total.day.minutes, my.dataframe$Churn)
table
mean(my.dataframe$Total.day.minutes)

matrix <- matrix(c(12, 6, 40, 42), ncol = 2, dimnames = list(c("Больше среднего", "Меньше среднего"),
                                                          c("Есть подписка", "Нет подписки")))

#Метод Хи-квадрат
chisq.test(matrix)

#Точный тест Фишера

table <- data.frame(my.dataframe$Total.day.minutes, my.dataframe$Total.eve.minutes)
table
mean(my.dataframe$Total.day.minutes)
mean(my.dataframe$Total.eve.minutes)

matrix <- matrix(c(8, 7, 7, 8), ncol = 2, dimnames = list(c("Больше среднего", "Меньше среднего"),
                                                           c("Минуты днём", "Минуты вечером")))
matrix
fisher.test(matrix)

#Тест МакНемара

mcnemar.test(matrix) 

#Тест Кохрана-Мантеля-Хензеля

my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 15)
my.dataframe <- data.frame(my.file)
table <- data.frame(my.dataframe$Total.day.minutes, my.dataframe$Churn, my.dataframe$Total.eve.minutes)
table
mean(my.dataframe$Total.day.minutes)
mean(my.dataframe$Total.eve.minutes)

matrix <-
  array(c(2, 6, 2, 5,
          3, 4, 1, 7),
        dim = c(2, 2, 2),
        dimnames = list(
          Время = c("Минуты днём", "Минуты вечером"),
          Подписка = c('Есть подписка', 'Нет подписки'),
          Относительно = c("Больше среднего", "Меньше среднего")))
matrix

mantelhaen.test(matrix) 

#Задание 10

my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 20)
my.dataframe <- data.frame(my.file)
str(my.dataframe)

#Сделаем таблицу исключительно из числовых значений
day.minutes <- my.dataframe$Total.day.minutes
day.calls <- my.dataframe$Total.day.calls
day.charge <- my.dataframe$Total.day.charge
eve.minutes <- my.dataframe$Total.eve.minutes
eve.calls <- my.dataframe$Total.eve.calls
eve.charge <- my.dataframe$Total.eve.charge
night.minutes <- my.dataframe$Total.night.minutes
night.calls <- my.dataframe$Total.night.calls
night.charge <- my.dataframe$Total.night.charge

suitable.dataframe <- data.frame(day.minutes, day.calls, day.charge, eve.minutes, eve.calls, 
                                 eve.charge, night.minutes, night.calls, night.charge)

#coloured correlation matrix
Mat <- cor(suitable.dataframe)
library(corrplot)

col4 <- colorRampPalette(c("#983232", "#db4c4c", "#ffa54c", "#ffff7f",
                           "#98ff98", "#a5ffd2", "#4ca5ff", "#4c4cff","#19198b")) 
corrplot(Mat, method = "pie", col = col4(20), cl.length = 20,
         order = "AOE",  tl.col = "black", addCoef.col = "#00cd00")

corrplot(Mat, method = "color", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black", addCoef.col = "#00cd00")

corrplot(Mat, method = "number", col = col4(20), cl.length = 21,
         order = "AOE",  tl.col = "black")
#VIF
library(car)
vif(lm(day.charge ~ day.minutes + day.calls + eve.charge + eve.minutes + night.charge + night.minutes, 
       data = suitable.dataframe))


#Задание 11
#Подготовка данных
my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 100)
my.dataframe <- data.frame(my.file)

day <- data.frame(duration = my.dataframe$Total.day.minutes, type= rep("day", 100))
eve <- data.frame(duration = my.dataframe$Total.eve.minutes, type= rep("eve", 100))
night <- data.frame(duration = my.dataframe$Total.night.minutes, type= rep("night", 100))
minutes <- rbind(day, eve, night)
minutes

is.factor(minutes$type)
minutes$type <- factor(minutes$type)
write.csv(minutes,"output.csv")
is.factor(minutes$type)
levels(minutes$type)
minutes$type <- relevel(minutes$type, ref = "day")

Means <- data.frame(duration = as.numeric(tapply(minutes$duration, minutes$type, mean)),
                    type = rep("Means", 3))
Means
minutes <- rbind(minutes, Means)
minutes$type <- relevel(minutes$type, ref = "Means")
layout(matrix(1:1, ncol = 1))

#Визуализация
stripchart(duration ~ type, data = minutes, pch = 19,
           col = c("black", "cadetblue2", "darksalmon", "darkseagreen"),
           ylab = "type", xlab = "minutes")
points(x = Means$duration, y = c(1, 1, 1), pch = 19,
       col = c("blue", "red", "green"))

#one-factor dispersion analysis
summary(aov(duration ~ type, data = minutes))

#linear model
M <- lm(duration ~ type, data = minutes)
summary(M)

#dispersion analysis table
anova(M)

#two-factor analysis
#Подготовка данных
my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 100)
my.dataframe <- data.frame(my.file)

str(my.dataframe)
voice_mail_1 <- my.dataframe[my.dataframe$Voice.mail.plan == 'Yes',]
str(voice_mail_1)
voice_1_day <- data.frame(duration = voice_mail_1$Total.day.minutes, type = rep("day", 26), voice_mail = rep("Yes", 26))
voice_1_eve <- data.frame(duration = voice_mail_1$Total.eve.minutes, type = rep("eve", 26), voice_mail = rep("Yes", 26))
voice_1_night <- data.frame(duration = voice_mail_1$Total.night.minutes, type = rep("night", 26), voice_mail = rep("Yes", 26))

voice_mail_0 <- my.dataframe[my.dataframe$Voice.mail.plan == 'No',]
str(voice_mail_0)
voice_mail_0 <- voice_mail_0[1:26, ]
voice_0_day <- data.frame(duration = voice_mail_0$Total.day.minutes, type = rep("day", 26), voice_mail = rep("No", 26))
voice_0_eve <- data.frame(duration = voice_mail_0$Total.eve.minutes, type = rep("eve", 26), voice_mail = rep("No", 26))
voice_0_night <- data.frame(duration = voice_mail_0$Total.night.minutes, type = rep("night", 26), voice_mail = rep("No", 26))

minutes <- rbind(voice_1_day, voice_1_eve, voice_1_night, voice_0_day, voice_0_eve, voice_0_night)
minutes
is.factor(minutes$type)
minutes$type <- factor(minutes$type)
is.factor(minutes$type)
levels(minutes$type)
is.factor(minutes$voice_mail)
minutes$voice_mail <- factor(minutes$voice_mail)
is.factor(minutes$voice_mail)
write.csv(minutes,"output.csv")
levels(minutes$voice_mail)

str(minutes)

#Визуализация
library(ggplot2)
ggplot(data = minutes, aes(x = type, y = duration)) + 
  geom_boxplot(aes(fill = voice_mail))
require(doBy)
summaryBy(duration ~ type + voice_mail, data = minutes,
          FUN = c(mean, sd, length))

plot.design(minutes)

#dispersion analysis table
M <- aov(duration ~ voice_mail + type + voice_mail:type, 
         data = minutes)
summary(M)

#linear model
M <- lm(duration ~ type*voice_mail, data = minutes)
summary(M)
anova(M)


#Задание 12 

my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 80)
my.dataframe <- data.frame(my.file)
str(my.dataframe)
state <- my.dataframe$State
day.minutes <- my.dataframe$Total.day.minutes
day.charge <- my.dataframe$Total.day.charge
dataset <- data.frame(state, day.minutes, day.charge)

plot(dataset$day.minutes, dataset$day.charge, xlim =  c(45, 350), ylim = c(0, 70), xlab = 'Minutes',
     ylab = 'Charge')

Model <- lm(day.charge ~ day.minutes - 1, data = dataset) # "-1" - для исключения свободного члена
summary(Model)

# Оценка доверительных интервалов регрессии
CPI.df <- cbind(predict(Model,interval ="conf"), predict(Model, interval ="pred"))  
CPI.df
CPI.df <- CPI.df[,-4] 
CPI.df
colnames(CPI.df) <- c("Y_fit","CI_l","CI_u","PI_l","PI_u")
head(CPI.df)

par(mfrow = c(1, 1))
matplot(dataset$day.minutes, CPI.df, type = "l", 
        lwd = c(2, 1, 1, 1, 1), col = c(1, 2, 2, 4, 4),
        ylab = "Charge", xlab="Minutes")
with(dataset, matpoints(day.minutes, day.charge, pch = 20))

# Оценка доверительных интервалов, параметрический подход

beta <- summary(Model)$coefficients[1] #0.17
beta
SE <- summary(Model)$coefficients[2] # 1.5*10^(-6)
SE
summary(Model)
ci.lower <- beta - qt(0.975, df = 79)*SE
ci.upper <- beta + qt(0.975, df = 79)*SE
upper <- c(ci.upper)
lower <- c(ci.lower)
value <- c(beta)
interval <- data.frame(Value <- value, Lower = lower, Upper = upper)
interval

# Оценка доверительных интервалов параметра бутстрепом
regr <- function(data, indices) {
  dat <- data[indices, ] 
  fit <- lm(day.charge ~ 0 + day.minutes, data = dat)
  return(summary(fit)$coefficients[1])
} 

library(boot)
results <- boot(data = dataset, statistic = regr, R = 1000)
results

plot(results)

quantile(results$t, c(0.025, 0.975)) 
(lower <- 1.49146)
(upper <- 1.562483)
# Чтобы избежать смещенных оценок
boot.ci(results, type = "bca")

# Оценка доверительных интервалов параметра методом имитаций
install.packages("arm")
library(arm)
simulations <- sim(Model, 1000)
hist(simulations@coef, breaks = 30)
sd(simulations@coef)
quantile(simulations@coef, c(0.025, 0.975))


# Оценка качества модели
summary(Model)
dataset$fit = fitted(Model)

library(ggplot2)

p1 = ggplot(dataset, aes(day.minutes, day.charge)) + geom_point() +
  geom_hline(aes(yintercept=mean(day.charge)), color = "blue") +
  geom_segment(aes(x = day.minutes, y = day.charge, xend = day.minutes, yend = mean(day.charge))) +
  ggtitle("Общая сумма квадратов TSS")

p2 = ggplot(dataset, aes(day.minutes, day.charge)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_segment(aes(x = day.minutes, y = day.charge, xend = day.minutes, yend = fit)) +
  ggtitle("Сумма квадратов остатков RSS")

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)




X <- my.dataframe$Total.day.minutes
Y <- my.dataframe$Total.day.charge 

# Подгоняем полиномы 1-4 порядков
allModel <- lapply(1:12,function(k) lm(Y ~ poly(X, k, raw = TRUE)))
extract <- function(fit) {
  sigma <- summary(fit)$sigma  # среднеквадратическаяя ошибка
  R2.adj <- summary(fit)$adj.r.squared  # скорректированный коэффициент R2 
  aic <- AIC(fit)           #  Информационный АIC-критерий
  out <- data.frame(sigma = sigma, R2.adj = R2.adj, AIC = aic)
  return(out)    }

result <- lapply(allModel, extract)
result <- as.data.frame(matrix(unlist(result), nrow = 12, byrow = T))

# Вычисление среднеквадратической ошибки кросс-валидации
M.ErCV <- sapply(1:12,function(k) {
  n <- length(X) ; Err_S  <- 0 
  for(i in 1:n)    { Xcv <- X[-i]
  lm.temp <- lm( Y[-i] ~ poly(Xcv, k , raw = TRUE))
  YR <- predict(lm.temp, newdata=data.frame(Xcv=X))[i]
  Err_S <- Err_S + (Y[i] -  YR)^2  }  
  sqrt(Err_S/n) })

# Вывод результатов в виде таблицы
result<-cbind(result, M.ErCV)
colnames(result) <- c("Ст. отклонение", "R2Adj", "Критерий AIC", "Ошибка кросс-валидации")
rownames(result) <- c("Полином 1 степени","Полином 2 степени",
                      "Полином 3 степени","Полином 4 степени", "Полином 5 степени", 
                      "Полином 6 степени", "Полином 7 степени", "Полином 8 степени",
                      "Полином 9 степени", "Полином 10 степени", "Полином 11 степени",
                      "Полином 12 степени")
result

# Визуализация
par(mfrow = c(1, 1))
plot(X, Y, type = "p", pch = 22, bg = "yellow", 
     xlab = "Minutes", ylab = "charge")
sapply(1:12, function(k) points(X,
                                predict(lm(Y ~ poly(X, k, raw = TRUE))),
                                type = "l", col = k, lwd = 2))
legend("topleft", c("k = 1", "k = 2","k = 3","k = 4",
                    "k = 5", "k = 6","k = 7","k = 8",
                    "k = 9", "k = 10","k = 11","k = 12"),
       col = 1:12, lwd = 2)


# Нелинейная логистическая модель

# Данные
data <- my.dataframe[1:20, ]
S1 <- data$Total.day.minutes
p1 <- data$Total.eve.calls
log.ss1 <- nls(p1 ~ SSlogis(S1, phi1, phi2, phi3))
summary(log.ss1)

# Доверительные интервалы нелинейной модели
Rsquared <- 1 - var(residuals(log.ss1))/var(p1)

# Рассчитанные по модели значения отклика
x <- 0:max(S1)
pr1 = predict(log.ss1, data.frame(S1 = x))


## заметим, что независимая переменная в SSlogis имеет имя "S1", 
## и поэтому мы изменяем имя столбца в таблице предикторных значений
### вычисляем ошибку регрессии линейной аппроксимацией
se.fit <- sqrt(apply(attr(pr1,"gradient"), 1, 
                     function(mat) sum(vcov(log.ss1)*outer(mat, mat))))
PCI <- pr1 + outer(se.fit, qnorm(c(.5, .025, .975)))

# Критические точки отклика
a <- coef(log.ss1)[1]*c(0.05, 0.5, 0.95)

# Критические точки фактора
plx<-approx(pr1, x, xout = a)$y

matplot(x, PCI,type="l", xlab = "Minutes",
        ylab = "Calls",
        lty = c(1,3,3), lwd = c(2,1,1), ylim = c(0,300))
matpoints(S1, p1, pch=20)
text(5, 5, bquote(R^2==.(round(Rsquared, 3))))
sapply(1:3, function(i)
  lines (c(0, plx[i], plx[i]), c(a[i], a[i], 0), lty = 2))

# Множественная регрессия
my.file <- read.csv(file.choose(), header = TRUE, sep = ",", nrows = 30)
my.dataframe <- data.frame(my.file)
str(my.dataframe)
M <- lm(Total.day.charge ~ Total.day.minutes + Total.day.calls,
        data = my.dataframe)
summary(M)
AIC(M)

# Шаговый регрессионный анализ (комбинированный подход на основе AIC)
Mstep <- step(M, direction = "both")
summary(Mstep)
AIC(Mstep)

# Регрессионные методы с регуляризацией

#to create Mat dataframe have to consist of numeric or integer values only
day.minutes <- my.dataframe$Total.day.minutes
day.calls <- my.dataframe$Total.eve.calls
day.charge <- my.dataframe$Total.day.charge
eve.minutes <- my.dataframe$Total.eve.minutes
eve.calls <- my.dataframe$Total.eve.calls
eve.charge <- my.dataframe$Total.eve.charge


suitable.dataframe <- data.frame(day.minutes, day.calls, day.charge, eve.minutes, 
                                 eve.calls, eve.charge)
str(suitable.dataframe)

# Лассо-регрессия
library(lars) #возможно, потребуется установить вручную
Xmat <- as.matrix(suitable.dataframe[, -1])
Xmat
M.las <- lars(Xmat, suitable.dataframe[, 1], type = "lasso")

par(mfrow = c(1, 2))
plot(M.las, plottype = "coefficients")
plot(M.las, plottype = "Cp")

# Параметр фракционирования S найдем перекрестной проверкой
set.seed(0)
par(mfrow = c(1, 1))
r <- cv.lars(Xmat, suitable.dataframe[, 1]) 
(bestfrac <- r$index[which.min(r$cv)])

(las.coef <- predict(M.las, Xmat, s = bestfrac, 
                     type = "coefficient", mode = "fraction"))

# Остатки модели
las.resid <- suitable.dataframe$close - predict.lars(M.las, Xmat, s = bestfrac,
                                                     type ="fit", mode = "fraction")$fit
rss.lasso <- sum(las.resid^2)/(nrow(suitable.dataframe) - 7)


# Гребневая регрессия
library(MASS)
M.ridge <- lm.ridge(day.charge ~ ., data = suitable.dataframe, lambda = seq(0,2,0.1))

plot(x = M.ridge$lambda, y = M.ridge$GCV, type = "o")

(lambda <- M.ridge$GCV[which.min(M.ridge$GCV)])

(M.ridge1 <- lm.ridge(day.charge ~ ., data = suitable.dataframe, lambda = lambda))

# Коэффициенты модели
beta.M.ridge1 <- coef(M.ridge1)
m <- length(beta.M.ridge1)
m

# Остатки модели
resid.ridge <- suitable.dataframe$Close - beta.M.ridge1[1] - 
  as.matrix(suitable.dataframe[,2:m])%*%beta.M.ridge1[2:m]
resid.ridge

# Число степеней свободы гребневой регрессии
d <- svd(as.matrix(suitable.dataframe[, 2:m]))$d
(df <- nrow(suitable.dataframe) - sum(d^2/(lambda+d^2)))

# Средний квадрат отклонений
(rss.ridge <- sum(resid.ridge^2)/df)

dataset$day.charge








































