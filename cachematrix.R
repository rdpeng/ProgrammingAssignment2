##This is Programming Assignment #2. In it we are creating two functions that will
##find the inverse of a matrix, a complex calculation that can be made simpler
##using the caching rules of lexical scoping found in R

## This first function is similar to the example function makeVector
## However in this case, I am switching around certain functions and variables
## to suit the nature of this project. Instead of numeric vectors, the function
## will take matrices, and instead of finding the mean, it will find the inverse,
## however it will set and get the various variables to the cache as in the
## makeVector function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
         }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This will solve for the inverse of the matrices, using cached data
## when and where available, as with the cachemean function in the example

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
         }
        data <- x$get()
        i <- mean(data, ...)
        x$setmean(i)
        i
}
