## Put comments here that give an overall description of what your
## functions do:
# These functions work together to calculate the inverse of a matrix and cache the result. Once the result 
# has been cached, if the same calculation is asked for again, rather than recalculating the value, the
# cached result is returned instead.

## Write a short comment describing this function
# Steps: 
#   1) Sets m to NULL to establish it as a placeholder for later use.
#   2) Creates a function (set) that uses the input (y) and assigns it equal to x in makeCacheMatrix's parent 
#       environment, using the <<- operator. 
#   3) Creates a function (get) which returns the value assigned to X
#   4) Creates a function (setmatrix) which sets "m" equal to the function solve, in makeCacheMatrix's parent 
#       environment, using the <<- operator. 
#   5) creates a function (getmatrix) which returns the value of m
#   6) Creates a list of the different functions created

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(
         set = set, 
         get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}



## Write a short comment describing this function
#   Returns a matrix that is the inverse of 'x'
#   The function first checks to see if the inverse has already been calculated. If so, it gets the inverse
#   from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the 
#   value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

