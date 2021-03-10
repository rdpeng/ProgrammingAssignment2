## Cache the matrix, optimizing resources

## Calculate the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() { x }
        setinvers <- function( solve ) { m <<- solve }
        getinvers <- function() { m }
        list( set = set, 
              get = get,
              setinvers = setinvers,
              getinvers = getinvers )
}

## Caches a matrix, inverse of "x"

cacheSolve <- function(x, ...) {
        m <- x$getinvers()
        
        if (!is.null(m)) {
                message("getting cached matrix")
                return (m)
        } 
        
        matX <- x$get()
        ## Calculate the inverse of a matrix
        m <- solve(matX, ...)
        x$setinvers(m)
        
        m
}

## Return a matrix that is the inverse of 'x'