## The functions in this code cache the inverse of a matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize the inverse property
  i <- NULL
  
  ## Setting the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Getting the matrix
  get <- function() {
    m
  }
  
  ## Setting the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Getting the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Returns the list
  list(set =  set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Computing the inverse of the matrix returned by "makeCacheMatrix".
## If the inverse has already been calculated,
## then the "cacheSolve" and the function will retrieve the inverse of the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Returns the inverse
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculating the inverse
    m <- solve(data) %*% data
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    m
}
