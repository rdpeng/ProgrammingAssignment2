## This is a script submitted by karanrughvani for Coursera R Programming assignment 2 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL                             ## initializing inv as null so that it holds value of matrix inverse 
    set <- function(y) {                    ## defining the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if a new matrix is entered, reset inv to NULL
    }
    get <- function() x                     ## get function returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                  
}


## The below function will compute the inverse of the "matrix" returned by makeCacheMatrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv