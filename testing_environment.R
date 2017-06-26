##Testing Environment
#Create a matrix
vec <- c(7,2,1,0,3,"-1","-3",4,"-2")
bsp <- matrix(data = as.numeric(vec), ncol = 3, nrow = 3)
bsp_inv <- solve(bsp)
det_bsp <- det(bsp)


#source bsp functions
source("makeVector.R")
source("cachemean.R")

#testing
a <- makeVector(bsp)
b <- cachemean(a)

#testing the new functions
source("makeCacheMatrix.R")
alpha <- makeCacheMatrix(bsp)

source("cacheSolve.R")
beta <- cacheSolve(alpha)
