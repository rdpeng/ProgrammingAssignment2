# Purpose: 
# Matrix inversion is usually a costly computation and there may be some benefits to
# caching computed results rather than computing it repeatedly.
# The following two functions cache and resuse the inverse of a matrix.

# Function: makeCacheMatrix creates a list containing a function to:
#    1. set the value of the matrix
#    2. get the value of the matrix
#    3. set the value of inverse of the matrix
#    4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function( y ) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function( inverse ) m <<- inverse
    getInverse <- function() m
    list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## Function: If exists, cacheSolve returns inverse of a matrix 'x' from cache.
## For the first time it computes the inverse of a matrix and stores it for future use.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {

    m <- x$getInverse()
    if( !is.null( m ) ) {
        message( "getting cached data." )
        return( m )
    }
    data <- x$get()
    m <- solve( data )
    x$setInverse( m )
    m
}

## Test run:
#x = rbind( c(1.00, -0.25, 0.50), c(0.50, -1.00, 0.25), c(0.25, -0.50, 1.00) )
#m = makeCacheMatrix(x)
#m$get()

## First run
#cacheSolve(m)

## Susequent run gets cached data
#cacheSolve(m)
