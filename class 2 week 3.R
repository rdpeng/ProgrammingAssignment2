# Qn 1
library(datasets)
data(iris)
?iris
View(iris)
virginica <- subset(iris, Species == "virginica")
virginica
summary(virginica)

# Qn 2
rowMeans(iris[, 1:4])
colMeans(iris)
apply(iris[, 1:4], 2, mean)
apply(iris, 2, mean)
?apply

# Qn 3
data(mtcars)
tapply(mtcars$cyl, mtcars$mpg, mean)
lapply(mtcars, mean)
mean(mtcars$mpg, mtcars$cyl)
with(mtcars, tapply(mpg, cyl, mean))
View(mtcars)

# Qn 4
x <- with(mtcars, tapply(hp, cyl, mean))
x
x[3] - x[1]

# Qn 5
debug(ls)
ls()