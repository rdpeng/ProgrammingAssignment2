## Creating a matrix object that can cache it's inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## function for setting the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## function for getting the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## function for setting inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## function for getting the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix 
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse 
  x$setInverse(m)
  
  ## Return the matrix
  m
}
