## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        k <- NULL 
        set <- function(z){
        x <<- z ##set input as matrix
        k <<- NULL ##set valve to null
        }
        get <- function()x
        setInverse <- function(inverse) k <<- inverse
        getInverse <- function() k 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix which is inverse of 'y'
        k <- x$getInverse()
        if(!is.null(k)){
        message("getting result which is cached")
        return(k)
        }
        dat <- x$get()
        k <- solve(dat,...)
        x$setInverse(k)
}
