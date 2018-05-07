
##test data
x <- 1:9
mat = matrix(x, nrow=3)

require(MASS)

matinv <- NULL

## this function takes a matrix, inverses it, stores it allowing for global use, and returns it

makeCacheMatrix <- function(x) {
        matinv <<- ginv(x)
        matinv
}


## this function will return the cached inversed matrix if it exists, otherwise it will inverse the matrix

cacheSolve <- function(y) {
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        matinv <<- ginv(y)
        matinv
}

makeCacheMatrix(mat)
cacheSolve(mat)
