## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                invcache <- NULL
                set <- function(y) {
                        x <<- y
                        invcache <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) invcache <<- inverse
                getinverse <- function() invcache
                list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                invcache <- x$getinverse()
                if(!is.null(invcache)) {
                        message("getting cached data.")
                        return(invcache)
                }
                data <- x$get()
                invcache <- solve(data)
                x$setinverse(invcache)
                invcache
}
