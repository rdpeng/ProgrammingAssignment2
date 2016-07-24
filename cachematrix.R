## The first function, `makeCacheMatrix` creates a special "vector" that containing a function to:
        ## set the matrix in cache
        ## get the matrix from cache
        ## set the inverse matrix into cache
        ## get the inverse matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
                setinverse <- function(inverse) m <<- inverse
                getinverse <- function() m
                list(set = set, 
                     get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## Cachesolve checks if the matrix is solved, if it is solved it returns the matrix,
## otherwise it inverts the matrix and sets it as 'setinverse' 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        
        # if the inverse has already been calculated
        
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)  # sets the value of the inverse in cache via the setinv function.
        m
}

