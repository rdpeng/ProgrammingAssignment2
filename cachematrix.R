## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## initialize result
    s <- NULL
   
    ## set the value of matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## get the value of matrix
    get <- function() x
    
    ## set inverse of matrix
    setinverse <- function(inverse) s <<- inverse
    
    ## get inverse of matrix
    getinverse <- function() s
    
    ## return result object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## function return inverse of matrix
cacheSolve <- function(x, ...) {
    ## get inverse of argument x
    result <- x$getinverse()
    
    ## if result exists - return it
    if(!is.null(result)) {
        message("getting cached data")
        return(result)
    }
    
    ## if matrix is new get it
    data <- x$get()
    
    ## inverse matrix - argument x
    result <- solve(data, ...)
    
    ## save inverse of x
    x$setinverse(result)
    
    ## Return a matrix that is the inverse of 'x'
    result
}
