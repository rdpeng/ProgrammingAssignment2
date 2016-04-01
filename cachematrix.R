## This function create the "special" matrix object
## cache it and inverse

## This constructor function for creating "special" matrix and cache it

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## This function check cache and inverse "special" matrix in empty cache case

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)){
        message("getting data from cache")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
    
    ## Return a matrix that is the inverse of 'x'
}
