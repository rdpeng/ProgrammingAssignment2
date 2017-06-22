##Testing Environment
#Create a matrix
bsp <- matrix(data = as.numeric(c(7,2,1,0,3,"−1","−3",4,"−2")), nrow = 3, ncol = 3)
bsp_inv <- solve(bsp)
vec <- c(7,2,1,0,3,"−1","−3",4,"−2")

#source bsp functions
source("makeVector.R")
source("cachemean.R")

#testing
a <- makeVector(vec)
b <- cachemean(a) ##doesnt work
