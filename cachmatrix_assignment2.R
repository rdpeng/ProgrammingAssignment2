##The first function, makeCacheMatrix creates a special "Matrix",
##which is really a list containing a function to
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse matrix
#4.get the value of the inverse matrix



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##The following function calculates the inverse of the special "Matrix" created with the above function. 
#However, it first checks to see if the inverse of a Matrix has already been calculated. 
#If so, it gets the inverse of a Matrix from the cache and skips the computation.
#Otherwise, it calculates the inverse of a Matrix
#and sets the the inverse of a Matrix in the cache via the setinverse function.


cacheSolve <- function(x , ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
