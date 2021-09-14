## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                  x <<- y
                  inv <<- NULL
        }
        get <- function() {x}
        SetInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, SetInverse = SetInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...){
            inv <- x$getInverse()
            if (!is.null(inv)) {
              message("I am getting cashed data")
              return(inv)
            }
            mat <- x$get()
            inv <- solve(mat, ...)
            x$SetInverse(inv)
            inv
}
