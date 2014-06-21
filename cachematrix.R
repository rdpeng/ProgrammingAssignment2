## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix creates an inverse of the given matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(m) m <<- solve(x) 
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
#CacheSolve Function checks if inverse exists, if yes it returns the cached matrix else it returns an inverse of the matrix

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        
        m <<- x$getinv()
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv()
        m
}
