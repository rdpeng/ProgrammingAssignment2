## This function creates and caches an inverse of a matrix

## It allows getting a matrix, inversing it and return the results


makeCacheMatrix <- function(x = matrix()) {
  ## Initializing the inverse attribute
  i <- NULL
 
  ## Sets the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Gets the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Sets the inverse the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Gets the inverse matrix
  getInverse <- function() {
    i
  }
  
  ## Lists the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## If a cache of the inverse matrix exists, it will use it, 
## otherwise it calculates the reverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Gets the matrix
  data <- x$get()
  
  ## Calculates the inverse matrix
  m <- solve(data) %*% data
  
  ## Sets the inverse maTRIX
  x$setInverse(m)
  
  ## ReturnS the matrix
  m
}
