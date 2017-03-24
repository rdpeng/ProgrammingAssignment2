## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special list in order to set the value of the matrix, get the value, set the inverse of the matrix and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)

}


## Write a short comment describing this function
## This function finds the inverse of the the invertible matrix by first checking whether the inverse was already calculated earlier in cache memory, if yes, it gets the value from there. If no, it calculates!
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
