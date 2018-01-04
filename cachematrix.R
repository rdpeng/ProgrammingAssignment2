## Creates a special matrix object 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL  ## Clears m everytime the function is called
  
  set <- function(y){  ## Creates a child environment
    x <<- y             ## Set the matrix to x in the child environment
    m <<- NULL          ## Clears m in the child environment
  }
  
  get <- function() x
  setMat <- function(matrix) m <<- matrix
  getMat <- function() m
  list(set = set, get = get, setMat = setMat, getMat = getMat)
  
}


## Returns the inverse of the special matrix created by makeCacheMatrix calculating it or retrieving the cached inverse matrix 
## when it is available.

cacheSolve <- function(x, ...) {
       
  m <- x$getMat()  ## Gets cached data, if any.
  
  if(!is.null(m)){   ## Checks if there is cached data
    message("getting cached data")
    return(m)
  }
  
  ## If there is no inverse matrix already stored in the cache memory, calculates it.
  
  data <- x$get()
  m <- solve(data, ...)
  x$setMat(m)
  m
}
