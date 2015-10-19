## This function creates a special matrix object that can cache its own inverse
## It is a square invertible matrix

## Create makeCacheMatrix() function
## This function will create a matrix that will cache its own inverse

makeCacheMatrix <- function(x = matrix()) {

  ## Step 1 - Set Matrix
  ## Step 2 - Get Matrix
  ## Step 3 - Set the Inverse Matrix
  ## Step 4 - Get the Inverse Matrix
  ## All of the above will be used as input for cacheSolve

  inv = NULL
  set = function(y) {
      ## '<<-' assigns a value to an object in an environment different from the current one
      x <<- y
      inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set,
       get=get, 
       setinv=setinv,
       getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  
  ## If the inverse has already been calulated, this step gets it from the cache and skips the computation
  if (!is.null(inv)){
    
      message ("getting cached data")
      return (inv)
  }
  
  ## Otherwise, it calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  ## Sets the value of the inverse in the cache via the setinv function
  x$setinv(inv)
  
  return(inv)
  
}