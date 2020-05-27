## the makeCacheMatrix function takes matrix as input. It can be used to get and set matrix and also get and set the inversed matrix.
## cacheSolve function takes makeCacheMatrix function as input. If there is any prior inversed matrix with the input,
## it gets the cached result. Else it calculates the result stores it in the setinverse function.

## used to get, set, get inverse and set inverse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
        ## setting the matrix and setting the inverse to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## get matrix
        get <- function() x
        ##set inverse
        setinverse <- function(inverse) i <<- inverse
        ## get inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculate inverse of a matrix and store it if not already cached.

cacheSolve <- function(x, ...) {
        ##getting the cached inverse
      i <- x$getinverse()
        ##returning the cached inverse if present
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ##get the input matrix
        data <- x$get()
        ## inverse the matrix
        i <- solve(data)
        ##caching the inversed matrix
        x$setinverse(i)
        ## returning the matrix
        i
}
