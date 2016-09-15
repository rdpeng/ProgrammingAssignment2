##Create a matrix and get the inverse of it
## Create a matrix

makeCacheMatrix <- function(x = matrix(data, nrow, ncol, byrow)) {
matrix(data, nrow, ncol, byrow)
}


## The inverse of a matrix

cacheSolve <- function(x, ...) {
library(MASS)
inv <- ginv(x)
inv %*% x
zapsmall(inv %*% x)
        ## Return a matrix that is the inverse of 'x'
}
