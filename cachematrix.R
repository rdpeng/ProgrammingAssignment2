## These functions create a matrix, inverse the matrix, cache the results, check cache for the same matrix to return results from cache

## This first function creates the matrix and sets the cache

makeCacheMatrix <- function(x) {
        s <- NULL
        set <- function(y) {
                x <<- y  ## sets the cache for x
                s <<- NULL  ## sets the cache for s
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve  ## writes to the cache for s
        getsolve <- function() s
        list(set = set, get = get,  ## this lists the environments of the variables
             setsolve = setsolve,
             getsolve = getsolve)
}
## This function creates the inverse if the matrix has not been run before
## If the matrix has been run, it gets the matrix from the cache

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                ## checks to see if the matrix has already been cached
                message("getting cached data")  ## informs the data is coming from the cache
                return(s)  ## returns the cahced data
        }
        data <- x$get()  ## sets data to the matrix of x
        s <- solve(data, ...)  ## the solve function inverses the data in x and applies to s
        x$setsolve(s)
        s  ## The s variable returns a matrix that is the inverse of 'x'
}
