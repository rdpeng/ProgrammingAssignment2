## Functions that cache the inverse of a matrix


## Creates a special matrix object which can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Matrix setting method
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Getting matrix method
  get <- function() {
    ## Returning matrix
    m
  }
  
  ## Matrix inversion method
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Getting the inverse of the matrix
  getInverse <- function() {
    ## Returning inverse property
    i
  }
  
  ## Returning the methods list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computing the inversion of matrix by "makecachematrix"
## cachesolve will retrieve the matrix from cache.
cacheSolve <- function(x, ...) {
  
  ## Returns matrix - inverse of 'x'
  m <- x$getInverse()
  
  ## returns the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## to get matrix object
  data <- x$get()
  
  ## Calculating matrix multiplication for the inverse
  m <- solve(data) %*% data
  
  ## setting inverse of the object
  x$setInverse(m)
  
  ## returns the matrix and their values
  m
}
