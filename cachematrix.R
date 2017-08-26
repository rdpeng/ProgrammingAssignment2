## The below pair of functions caches inverse of matrix 

## This function creates a matrix object that can cache its inverse.
## it supports 4 functions i.e. 1. setting matrix, 2. getting matrix, 
## 3. setting inverse and  4. getting inverse

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(invers) inverse <<- invers
    getinverse <- function() inverse
        
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by  
## makeCacheMatrix function above. If the inverse has already 
## been calculated, then the cacheSolve function been calculated and 
## it's the same matrix, then it will retrieve the inverse from the 
## cache memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
