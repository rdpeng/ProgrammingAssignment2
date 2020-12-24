## Two functions that cache the inverse of a matrix: makeCacheMatrix and cacheSolve

## First function: Creates a special matrix object that can cache its inverse. I have just replaced the word "matrix" instead of "vector" in the original code to make it work.
makeCacheMatrix <- function(x = matrix()) {
        ## 1. Assign the inverse property
        m <- NULL
        ## 2. Method to set the matrix (replace "matrix" on the original "y")
        set <- function(y) {
                x <<- y
                m <<- NULL}
        ## 3. Method the get the matrix
        get <- function() {x} 
        ## 4. Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                m <<- inverse}
                ## 5. Method to get the inverse of the matrix
        getInverse <- function() {m}
        ## 6. Return a list of the methods
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}

## Second function: solves the cache problem by returning the inverted matrix of 'x' by executing the previous code.
## Notice the use of "..." for efficiency.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        ##Returns the inverse matrix. If not availabe, returns message and calculates the inverse:
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setInverse(m)
        m
}
