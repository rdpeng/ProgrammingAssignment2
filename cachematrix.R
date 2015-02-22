## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m2 <- NULL
    set <- function(y) {
        x <<- y
        m2 <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m2 <<- solve
    getsolve <- function() m2
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m2 <- x$getsolve()
    if(!is.null(m2)) {
        message("getting cached solve data")
        return(m2)
    }
    data <- x$get()
    m2 <- solve(data)
    x$setsolve(m2)
    m2    
}
