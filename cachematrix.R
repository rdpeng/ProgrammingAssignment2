## These functions help cache the inverse of a matrix. In case it has already bên calculated, it will get the cached data 
## and there is no need to recompute, which is a costly process.

##  This function creates a special "matrix" object that can cache its inverse, which is really a list of small functions. 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(set=set,get = get, setInverse=setInverse,
                getInverse=getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setInverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
