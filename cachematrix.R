## Matrix inversion is usually a costly computation and there may be some benefits to caching the inverse of a matrix,rather
## than compute it repeatedly.The following pair of functions cache the inverse of a matrix
## 1.makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse. It is a list of function  
## designed to do the following operation
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the given matrix
## get the value of the inverse of the given matrix


makeCacheMatrix <- function(x = matrix(x <- c(), ncol = sqrt(length(x)))) {
m = null
set <- function (y){
x <<- y
m <<- NULL
}
get <- function()x
setinverse <- function (solve) m <<- inverse
getinverse <- function() m
list ( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## 2.cacheSolve : This function computes the inverse of the special "matrix" returned by the makeCacheMatrix. If the inverse has 
## already been calculated (and the matrix had not changed), then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)){
        message("getting cached data")
        return (m)
        }
        data <- x$get()
        m <- inverse(data,...)
        x$setinverse(m)
        m
        }
}
