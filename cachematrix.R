## Put comments here that give an overall description of what your
## functions do

# This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function( y ) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function( inverse ) m <<- inverse
  getInverse <- function() m
  #list of all methods
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


# If exists, this function returns inverse of a matrix 'x' from cache.
# For the first time it computes the inverse of a matrix and stores it for future use.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  #if the matrix is already chaced:
  if( !is.null( m ) ) {
    message( "Cached data!" )
    return( m )
  }
  #else, solve it:
  data <- x$get()
  m <- solve( data )
  x$setInverse( m )
  m
}


## Test run:
#x = rbind( c(0.20, -0.50, -0.50), c(0.30, 1.00, 0.50), c(0.10, 1.00, 0.70) )
#m = makeCacheMatrix(x)
#m$get()
## First run
#cacheSolve(m)
## Susequent run gets cached data
#cacheSolve(m)
