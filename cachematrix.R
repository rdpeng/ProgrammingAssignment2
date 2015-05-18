## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that caches its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    # initialize catch
    matrixcache <- NULL
    set <- function(y) {
        x <<- y
        #empty catche
        matrixcache <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) matrixcache <<- solve
    getinverse <- function() matrixcache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #If the inverse has already been calculated (and the matrix has not changed), 
    #then the cachesolve should retrieve the inverse from the cache. 
    matrixcache <- x$getinverse()
    if(!is.null(matrixcache)) {
        message("getting cached data")
        return(matrixcache)
    }
    data <- x$get()
    matrixcache <- solve(data, ...)
    x$setinverse(matrixcache)
    matrixcache
}

