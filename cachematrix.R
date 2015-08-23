## makeCacheMatrix function creates special matrix object that caches inverse
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        ## sets value of matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ## gets value of matrix
        get <- function() x

        ## sets value of inverse
        setinverse <- function(inverse) m <<- inverse

        ## gets value of inverse
        getinverse <- function() m

        ## return list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve function computes inverse of special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {

        ## gets inverse of matrix from cache
        m <- x$getinverse()

        ## returns inverse from cache if already exists
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## gets value of matrix
        data <- x$get()

        ## calculates inverse of matrix
        m <- solve(data, ...)

        ## sets inverse of matrix
        x$setinverse(m)

        ## return matrix m that is inverse of x
        m
}
