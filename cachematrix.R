## The functions below allow to print inverse of a matrix if the inversion has been 
## calculated, otherwise calculated now.

## The first function, makeCacheMatrix is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the solve/inversion
# get the value of the solve/inversion


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv<- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The following function calculates the inversion of the special "matrix" created 
# with the above function. However, it first checks to see if the inverse has already
# been calculated. If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the inverse in the cache via
# the setmean function.

cacheSolve <- function(x, ...) {
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