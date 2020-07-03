## These functions store the inverse of a matrix in order to maximize data 
## processing, preventing the same matrix from being computed repeatedly.

## Create special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function(){x}
     setInverse <- function(inverse) {inv <<- inverse}
     getInverse <- function() {inv}
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse "matrix" returned from the previous function, retriving 
## it´s previous computed inverse from the cache

cacheSolve <- function(x, ...) {
     inv <- x$getInverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
}
