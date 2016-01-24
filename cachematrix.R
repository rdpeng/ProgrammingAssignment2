## Week 3
#  Programming Assignment

# This function creates a special "matrix" object that can cache its inverse.
# Argument x must be a matrix
# $set - sets the matrix
# $get - gets the matrix
# $setInverse - sets the inverse of the matrix
# $getInverse - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # Check argument class
        if (class(x)!="matrix"){
                print("Argument is not a matrix")
        }
        else{
                X.Inverse <- NULL
                set <- function(y) {
                        x <<- y
                        X.inverse <<- NULL
                }
                get <- function() x
                setInverse <- function(inverse) X.Inverse <<- inverse
                getInverse <- function() X.Inverse
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
        X.Inverse <- x$getInverse()
        if(!is.null(X.Inverse)) {
                message("Getting cached data")
                return(X.Inverse)
        }
        data <- x$get()
        X.Inverse <- solve(data, ...)
        x$setInverse(X.Inverse)
        X.Inverse
}
