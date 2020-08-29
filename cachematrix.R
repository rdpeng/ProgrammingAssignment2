## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse)
        getInverse <- function(){inv}
        list(set=set, get=get, setInverse=setInverse, getInverse= getInverse)
}

        


## Create Inverse of matric from cache
cacheSolve <- function(x, ...) {
        inv <- x$getInverse(x)
        if(!is.null(inverse)) {
                message("Obtaining cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}
