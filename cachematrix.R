## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function..

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL                                  ## Initialise variable "s" as NULL
    set <- function(y) {                       ## Initialise setter "set" to set the value of the vector
        x <<- y
        s <<- NULL
    }
    get <- function() x                        ## Initialise getter "get" to get the value of the vector
    setsolve <- function(solve) s <<- solve    ## Initialise setter "setsolve" to set the value of the solve
    getsolve <- function() s                   ## Initialise setter "getsolve" to get the value of the solve
    list(set = set, get = get,                 ## Compile information about "vector" into a list 
         setsolve = setsolve,
         getsolve = getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    s <- x$getsolve()                          ## Check is variable "s" equal NULL or not
    if(!is.null(s)) {                          ## If "not" - return cached invert matrix result
        message("getting cached data")
        return(s)
    }
    data <- x$get()                            ## If "yes" - using the getter "get" to get value of the matrix
    s <- solve(data, ...)                      ## Making calculations to invert matrix
    x$setsolve(s)                              ## Using setter "setsolve" - save result in a Cache
    s                                          ## Printing result to screen
}
