#Необходимые пакеты
install.packages('class')
install.packages('caret')
install.packages('lattice')
install.packages('gridExtra')
install.packages('ggplot2')

library(class)
library(caret)
library(lattice)
library(gridExtra)
library(ggplot2)

#Подключение данных и задание сида
data(iris)
set.seed(1488)

#Графики
a <- qplot(Sepal.Length, Sepal.Width, data = iris) +
  facet_grid(facets = ~ Species) +
  geom_smooth(color = "green", se = FALSE) 
b <- qplot(Petal.Length, Petal.Width,data = iris) +
  facet_grid(facets = ~ Species) +
  geom_smooth(color = "green", se = FALSE)

#Создание и обучение выборки
names(iris)
iris$Species <- factor(iris$Species) 

index <- sample(1:nrow(iris), round(0.7*nrow(iris)))
train <- iris[index, ]
test <- iris[-index, ]
contrl <- trainControl(method = "repeatedcv", repeats = 7)
train(Species ~ ., data = iris, method = "knn", 
      trControl = contrl, preProcess = c("center", "scale"),
      tuneLength = 20)
knn.iris <- class::knn(train = train[, -5], test = test[, -5], 
                       cl = train[, "Species"], k = 7, prob = TRUE)
#Вывод результатов
grid.arrange(a, b, nrow = 2)
table(Фактические_данные = test$Species, Прогноз = knn.iris)
Acc <- mean(knn.iris == test$Species)
paste("Точность= ", round(100*Acc, 2), "%", sep = "")

