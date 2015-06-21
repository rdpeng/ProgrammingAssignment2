## Caching the inverse matrix
## makeCacheMatrix creates a matrix like object whose interface 
## its composed of 4 functions (get, set, getInverse and setInverse).
## Use always this functions to access matrix data.

## This function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x  
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
    
}


## Solving the inverse:
## If it's cached it returns the cached data, else
## it is calculated.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
