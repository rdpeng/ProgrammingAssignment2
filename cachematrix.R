## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
    ##  1. set the matrix
    ##  2. get the matrix
    ##  3. set the inverse
    ##  4. get the inverse
    inv = NULL
    set = function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Inverse the matrix created with the above function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ## If the inverse has already been calculated
    if(!is.null(inv)){
    ## Get the inverse from the cache
        message("getting cached data")
        return(inv)
    }
    ## Otherwise calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    ## Set value of the inverse in the cache
    x$setinverse(inv)
    inv
}
