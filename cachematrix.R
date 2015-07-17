## These two functions allow you to specify a matrix and calculate its inverse. 
## Only invertible matrices can be entered into the functions. Once the inverse
## is calculated, it is cached, so that when you ask for the inverse again, it
## does not recalculate the inverse (which can take a long time for large
## matrices), but instead retrieves the cached inverse. However, if the matrix
## has been changed, the inverse is calculated again.

## `makeCacheMatrix` allows you to input a matrix, and outputs a list of four 
## functions, that can then be used to gather information about the submitted
## matrix and its inverse. 

## The `set` function can be used to make a new matrix. The old x value is 
## replaced by the new y value and if an inverse was cached it is reset to NULL.

## The `get` function can be used to see the value of the matrix.

## The `setInverse` function should not be called by the user, but is used by 
## the `cacheSolve` function to store the inverse.

## The `getInverse` function is used by `cacheSolve` to check if the inverse
## of the specified matrix has already been calculated.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function() {
                inv
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## `cacheSolve` first checks if the inverse of the specified matrix has 
## already been calculated. If that is the case, it returns `inv`, which has
## stored the value of the inverse using the `getInverse` function. If the
## inverse is not yet calculated, it uses `get` to read the matrix and then
## `solve` to calculate the inverse. The inverse is cached using `setInverse` 
## and is then returned to the user.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
