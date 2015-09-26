## Programming Assignment 2 to conserve resources by 
## caching function outputs

## makeCacheMatrix creates and caches the inverse of a matrix
makeCacheMatrix <- function(x) {
    cm <- NULL
    set <- function(y) {
        x <<- y
        cm <<- NULL
    }
    get <- function() x
    setmatrix <- function() cm <<- solve(x)
    getmatrix <- function() cm
    list(set = set, get = get, setmatrix = setmatrix, 
         getmatrix = getmatrix)
}

## cacheSolve retrieves the cached matrix
cacheSolve <- function(x, ...) {
    cm <- x$getmatrix()
    if (!is.null(cm)) {
        message("getting cached matrix")
        return(cm)
    }
    data <- x$get()
    cm <- solve(data, ...)
    x$setmatrix()
    cm
}
  
  ## Return a matrix that is the inverse of 'x' - these are some tests
#a <- matrix(data = 1:4, nrow = 2, ncol = 2)
#b <- makeCacheMatrix(a)
#cacheSolve(b)
