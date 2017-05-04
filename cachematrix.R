
## 2 functions to create a special matrix object that can cache its inverse
## for future possible use. Speeds subsequent processing by used the cached version
## instead of calculating the inverse every time the function is invoked.

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Create 4 functions to be passed into a subsequent function:
  ## 1 - $set:  Set original matrix into global environment, resets placeholder
  ## 2 - $get:  Gets original matrix - passed parameter into function
  ## 3 - $setinverse: Sets inverse of the matrix
  ## 4 - $getinverse: Gets the inverse matrix (will be NULL If not previously run, popluated if run)

  inv_matrix <- NULL
  
  ##1 $set
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  ##2 $get
  get <- function() x
  
  ##3 $setinverse
  setinverse <- function(inv) inv_matrix <<- inv
  
  ##4 $getinverse
  getinverse <- function() inv_matrix
  
  ## returns a list with the function set
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the original matrix, using functions returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of a stored matrix created by the makeCacheMatrix function
  ## and then use the used cached version if available
  
  ## get cached inverse
  inv_matrix <- x$getinverse()
  
  ## if the cached inverse is NOT NULL, then pull the cached data because it's already been run
  ## return the inverse if it is available
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  ## if the inverse has not already been run then get the original matrix, solve for the inverse,
  ## then set the inverse for future use (cache)
  ## return the result
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinverse(inv_matrix)
  inv_matrix
  
}

