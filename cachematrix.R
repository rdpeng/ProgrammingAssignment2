## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<-NULL
        }
        get <- function() x
        setinv <- function(inv) i<<- inv
        getinv <- function() i
        list(set=set , get = get, setinv=setinv, getinv = getinv)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data) %*% data
        x$setinv(i)
        i
}
