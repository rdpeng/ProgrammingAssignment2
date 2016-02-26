## Pair of functions to caching the inverse value of matrix


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    imtrx <- NULL
    set <- function(y) {
        x <<- y
        imtrx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) imtrx <<- inverse
    getinverse <- function() imtrx
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## this function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    if (is.null(x)) {
        return(x)
    }
    imtrx <- x$getinverse()
    if(is.null(imtrx)) {
        imtrx <- solve(x$get())
        x$setinverse(imtrx)
    }
    imtrx
    ## Return a matrix that is the inverse of 'x'
}
