## Because Matrix inversion is a costly computer action caching 
## has got interest in order to prevent repeted computing.

## makeCacheMatrix creates a special "matrix" object that cacehs its own inverse

makeCacheMatrix <- function(x = matrix()){
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function creates the inverted matrix or inputs the result if it was previously computed in the environment

cacheSolve <- function(x, ...){
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <-solve(m, ...)
    x$setinverse(i)
    i
}
