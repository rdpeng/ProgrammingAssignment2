## makeCacheMatrix function creates a special 
## "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
## When the matrix is changed, rewrite changed matrix 
## and reset the inverse as NULL, and prepare for next
## computation
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    
## Cache the inverse by using the superassignment
## when the inverse is calculated
## Function setinverse rewrite the variable Inv 
## in the enviroment of function makeCacheMatrix
    setinverse  <- function(inverse) Inv <<- inverse 
    getinverse  <- function() Inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    Inv <- x$getinverse ()
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve retrieve the inverse from the cache
    if(!is.null(Inv)) {
        message("getting cached matrix")
        return(Inv)
    }

## If the inverse is not calculated
## then get the matrix and calculate it's inverse
    data <- x$get()
    Inv <- solve(data, ...)
    x$setinverse(Inv)
## Return a matrix that is the inverse of 'x'
    Inv
}