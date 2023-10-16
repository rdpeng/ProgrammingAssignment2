## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse matrix to NULL
  i <- NULL
  
  ## Function to set the matrix with a new value
  set <- function( matrix ) {
    x<<- matrix
    ## When a new matrix is set, invalidate the cached inverse
    i <<- NULL
  }
  
 ## Function to get the matrix
  get <- function() x
  
  ## Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    i<<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function(){
    ## Return the inverse property 
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function to get the cache inverse of the special matrix returned by 
## "makeCacheMatrix" above.
cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  m <- x
  inv <- solve(m, ...)
  x<<- m
  m <- x$getInverse()
  
  ## Just return the inverse if its already set
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix from our object
  data <- x$get()
  
  ## calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix 
  m
    
}

