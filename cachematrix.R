## Put comments here that give an overall description of what your
## functions do
#this function set the value of the matrix, get the value of the matirx, set the value of the solve and get the value of the solve
## Write a short comment describing this function

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
             getsolve = getsolve)
}


## Write a short comment describing this function
##The following function calculates the solve of the given "matrix" created with the above function. If so, it gets the solve from the cache and skips the computation.
##Otherwise, it calculates the solve of the data and sets the value of the solve in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
