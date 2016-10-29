## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## init matrix
        m <- NULL 
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
       ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse using solve
        setinverse <- function(solve) m <<- solve
        ## get the value of the inverse using solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## create a matrix and calls get inverse
        m <- x$getinverse()
        ## checks if matrix has been calculated already
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                ## if matrix has been calculated it returns cached matrix
        }
        ## otherwise gets matrix
        data <- x$get()
        ## calculates inverse value of matrix
        m <- solve(data, ...)
        ## sets the inverse value of matrix
        x$setinverse(m)
        ## returns inverse matrix
        m

}
