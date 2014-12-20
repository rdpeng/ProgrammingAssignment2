## CONTEXT:
# This was an assignment to write a pair of functions that caches
# the inverse of a matrix.
#
# By the time, we were studying lexical scoping of R.
#
# It was also useful to incite some discussion about ways to avoid
# repeating costly computations or caching benefits.
#
# The chosen naming approach emphasizes the method used to evaluate
# the inverse of a matrix.
#
# Since there are many different ways of doing this computation,
# it makes clear which of them was indeed used.


## This function creates a special "matrix" object that can cache
# its inverse.
# It is a list contining a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set,
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## This function computes the inverse of the special "matrix"
# returned by "makeCacheMatrix".
# If the inverse has already been calculated (and the matrix
# has not changed), then the cachesolve should retrieve the
# inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
