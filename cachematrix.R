## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Assignment: Caching the Inverse of a Matrix
  ## @x: a square invertible matrix
  ##  makeCacheMatrix: This function creates a 
  ##  special "matrix" object that can cache its inverse.
  ## return: a list containing functions to
  ##             set and get the matrix
  ##             set and get the inverse
  ##         this list is used as the input to cacheSolve() - reminder
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value in different  
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## return: inverse of the original matrix input to makeCacheMatrix()
  ## cacheSolve: This function computes the inverse of the special "matrix" 
  ## returned by makeCacheMatrix above. If the inverse has already been 
  ## calculated (and the matrix has not changed), 
  ## then the cachesolve should retrieve the inverse from the cache.
  
  inv = x$getinv()
  
  # if the inverse has been calculated
  if (!is.null(inv)){
    # get it from the cache 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets value of inverse in cache via setinv function.
  x$setinv(inv)
  
  return(inv)
}


