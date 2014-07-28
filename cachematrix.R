## Test codes:
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## A<-hilbert(4)
## A
## x<-makeCacheMatrix(A)
## x
## y<-cacheSolve(x)
## y

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv<<-inverse
    getInverse <- function() inv
    list(set = set, get = get,setInverse = setInverse,getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setInverse(inv)
    inv
}
