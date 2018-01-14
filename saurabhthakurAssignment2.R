# a set of two functions is written create cache matrix and to calculate 
# the inverse of invertible matrix
# 
#
# makeCacheMatrix - creates a matrix object that can cache its inverse 
#
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                # <<- is the superassignment operator, to assign value
                # in a different env than current env.
                x <<- y
                i <<- NULL
        }
        # assign the function, 'x' is in the new env created by function
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        #this will allow using $ 
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

#cacheSolve - it calculates the inverse of matrix from makeCacheMatrix
#if the inverse is already calculated then it retrieves from cache
# else it calculates the inverse.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        #return the inverse if it is already available
        if (!is.null(i)) {
                message("i is not null, getting cached data")
                return(i)
        }
        #else calculate the inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        #last called value to return
        i
}