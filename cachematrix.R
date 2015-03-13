## Put comments here that give an overall description of what your
## functions do
## For a very big matrix, it may take too long to compute the inverse,
## especially if it has to be computed repeatedly (e.g. in a loop).
## If the contents of a matrix are not changing, it may make sense to cache
## the value of the inverse so that when we need it again, it can be looked up
## in the cache rather than recomputed.

## Write a short comment describing this function
## This function introduces the <<- operator which can be used to assign a value
## to an object in an environment that is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## This function calculates the inverse of the special "matrix" created with the function makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
