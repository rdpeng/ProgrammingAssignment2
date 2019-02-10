## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## setInverse set the value of the inverse
## getInverse get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y){
     x <<- y
     inv_x <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_x <<- inverse 
  getInverse <- function() inv_x
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getInverse()
  if (!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  mat_x <- x$get()
  inv_x <- solve(mat_x, ...)
  x$setInverse(inv_x)
  inv_x
}
