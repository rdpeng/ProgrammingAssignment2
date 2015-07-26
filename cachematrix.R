## Checks to see if there is an inverse of the matrix stored,
## then returns the cached matrix if so. If there is not a cached matrix
## calculates and returns the invers of the matrix.

## create a cached matrix and define the functions used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set<- function (y){
        x<<- y
        m<<- NULL
    }
    get <-function () x
    setinverse<- function (inv) m
    getinverse<- function () m
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Checks the cache for the inverse of the matrix, or calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    y<- x
    
    x<- x$getinverse
    if (!is.null(m)){
        message ("getting cached data")
        return (m)
    }
    data<- x$get
    m<- inv(data, ...)
    x$setinverse(m)
    
}
