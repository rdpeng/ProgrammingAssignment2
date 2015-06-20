## These two functions are used to compute the inverse of a given matrix. 
## If the inverse is cached, it returns the cache, otherwise it inverts the matrix and
##   caches the inverse before returning the inverse of the given matrix.

## Creates a list containing four functions:  Two functions to cache and retrieve a matrix
##   and two functions to cache and retrieve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse_m <-NULL
    set <- function(y) {            ##cache matrix and clear inverse matrix
        x <<- y
        inverse_m <<-NULL
    }
    get <- function() {             ##retrieve matrix from cache
        x
    }                 
    setinverse <- function(solve){  ##cache inverse matrix
        inverse_m <<- solve
    } 
    getinverse <- function() {      ##retrieve inverse matrix from cache
        inverse_m
    }
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computes and caches the inverse of the special "matrix" if one does not exist
##   and returns inverse matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_m <- x$getinverse()
    if(!is.null(inverse_m)) {               ##retrieve inverse from cache if stored
        message("getting cached data")
        return(inverse_m)
    }
        else{                               ##retrieve matrix, compute and store inverse
            data <- x$get()
            inverse_m <- solve(data, ...)
            x$setinverse(inverse_m)
            inverse_m
        }
}
