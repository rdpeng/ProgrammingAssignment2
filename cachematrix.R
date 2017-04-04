## Put comments here that give an overall description of what your
## functions do

## These functions combined call the cache (if available) the inverse of a matrix value. If not defined it will execute and set
## cache value for future use. 

## Write a short comment describing this function
## This portion of the function is setting the behaviour to creat a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL 
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## This function will produce the inverse of the matrix. If the inverse has not been computed, it will do so and set the value in the cache.
## If the result has already been cached it will provide cached data along with message.
cacheSolve <- function(x, ...) {
        i<- x$getinverse() 
        if(!is.null(i)) {
                message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    
}
