## Functions calculates the inverse of matrix n x n and once calculated
## keeps in cache

#makeCacheMatrix function creates the list containing 4 funtions, that:
#1. set the matrix
#2. get the matrix
#3. set the inversed matrix
#4. get the inversed matrix
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

#cacheSolve function checks if there is inversed matrix already solved and if not
#calculates it and saves it in the cache via setsolve function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data for inversed matrix")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
