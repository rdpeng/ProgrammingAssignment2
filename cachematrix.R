## This function cache  the inverse of a matrix.
## We assume that the matriz are always inversible.

## makeCacheMatrix is a spacial function that creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## m is the matrix with inverse
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }    
    
    get <- function() x
    
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list( set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse )    
}

## 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(a = data, ... = ...)
    x$setInverse(m)
    
    m
}

