## Creating 2 functions that gets the inverse of an input square matrix.
## If the same input is provided, the function will return the cached 
## value of the inverse computed earlier.

## Below function is to intialize the matrix and inverse. 
## It also contains sub-functions that returns the value of the 
## current inverse cached and the matrix provided as input.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                 x <<- y
                  i <<- NULL
         }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Computes the inverse of the matrix provided or displays the
## cached inverse computed earlier.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
           message("getting cached data")
           return(i)
        }
        data <- x$get()
        i <- solve(data,diag(nrow(data)))
        x$setinverse(i)
        i
}
