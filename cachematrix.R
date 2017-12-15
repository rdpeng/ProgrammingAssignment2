## The following functions are able to cache the inverse of a matrix.

## 1.  `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## 2.  `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix 
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<-NULL
  }
  get <- function() x
  setInvMatrix <- function() inv <<- solve(x)
  getInvMatrix <- function() inv
  list(set = set,
       get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setInvMatrix`
## function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
  inv <- x$getInvMatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInvMatrix(inv)
  inv
}