## This script uses the cacheSolve() function to calculate the inverse of a 
## given matrix made with the makeCacheMatrix() function
## If the calculation has already been performed it will fetch the result
## from memory

## Function to made the matrix object

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    ## Define getter and setter functions
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    ## Define getter and setter functions for the inverse
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Function to invert the matrix, if cached it will fetch the result from memory
## NOTE THAT MATRIX MUST BE THE OBJECT CREATED IN THE makeCacheMatrix function!

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
