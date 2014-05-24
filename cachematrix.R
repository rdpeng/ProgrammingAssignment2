## Here are two functions for ProgrammingAssignment2
## The first function creates 
## a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # create functions: set, get, setinverse, getinverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x # on get, print x
    setinverse <- function(solve) i <<- solve # on setinverse, set i for ref
    getinverse <- function() i # print i
    # put functions into list object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function calculates the inverse of 
## the special "matrix" created with the above function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # try to get the inverse of x
    i <- x$getinverse()
    # if it exists, return it and exit function
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # otherwise, take data from x and use solve function to calculate inverse
    data <- x$get()
    i <- solve(data, ...)
    # call setinverse to cache inverse for future reference
    x$setinverse(i)
    # return inverse
    i
}
x <- matrix(1:16,4,4)
