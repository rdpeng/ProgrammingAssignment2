## Put comments here that give an overall description of what your
## functions do
#this function set the value of the matrix, get the value of the matirx, set the value of the solve and get the value of the solve
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(value) inv <<- value
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
##The following function calculates the solve of the given "matrix" created with the above function. If so, it gets the solve from the cache and skips the computation.
##Otherwise, it calculates the solve of the data and sets the value of the solve in the cache via the setsolve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(inv)
        inv
}
