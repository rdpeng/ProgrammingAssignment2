## The following functions compute the inverse of a matrix,
# and catch the inverse if it is already computed

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsole)
}
}


# This function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the mean from 
# the cache and skips the computation.
# If not, it computes the inverse and sets the value in the cache via
# the setsolve function.

cacheSolve <- function(x, ...) {
       
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}}
