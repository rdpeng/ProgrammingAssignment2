################# Function 1: makeCacheMatrix() 
## This function creates a special "matrix" object that can cache its inverse.
## 

makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set matrix
  ##              2. get matrix
  ##              3. set the inverse matrix
  ##              4. get the inverse matrix
  ##         this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

################# Function 2: cacheSolve()
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##   If the inverse has already been calculated (and the matrix has not changed), 
##     then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  return(inv)  
}

