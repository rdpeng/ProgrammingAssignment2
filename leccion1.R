library(foreign)
datos<-read.table("datos1.csv", sep=",", header=TRUE)
datos
class(datos)
dim(datos)
rownames(datos)
colnames(datos)
sum(is.na(datos$Solar.R)) ##suma número de NA

In general, if the result is a list where every element is of length one, then sapply() returns a vector. If the result is a list where every element is a vector of
| the same length (> 1), sapply() returns a matrix. If sapply() can't figure things out, then it just returns a list, no different from what lapply() would give you.
sum(flags$orange)
flag_colors<- flags[,11:17]
lapply(flag_colors, sum)
sapply(flag_colors, mean)
flag_shapes<-flags[,19:23]
lapply(flag_shapes, range)
shape_mat<-sapply(flag_shapes, range)
cls_vect<- sapply(flags, class)
lapply(unique_vals, function(elem) elem[2])

datos2 <- read.table("spectdata", sep=",", header=TRUE)
