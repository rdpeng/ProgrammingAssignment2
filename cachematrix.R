## Line 6 starts the makeCacheMatrix function that stores the output of the cacheSolve 
## function from the variable i. The vector container must be created first.
## Line 24 starts the cacheSolve function that provides the inverse of the given matrix (x)

## I changed the vaiables from the original mean cache to solve and substituted i for m.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) i <<- solve
        getSolve <- function () i
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## Similar to above, I changed out the general mean function to solve and made sure
## the variables matched the container above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getSolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setSolve(i)
        i
}
