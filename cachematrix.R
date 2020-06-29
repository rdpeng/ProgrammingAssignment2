## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        iNv <- NULL
        set <- function(y) {
                x <<- y
                iNv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) iNv <<- solve
        getinv <- function() iNv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iNv <- x$getinv()
        if(!is.null(iNv)) {
                message("getting cached data")
                return(iNv)
        }
        data <- x$get()
        iNv <- solve(data, ...)
        x$setinv(iNv)
        iNv
}
