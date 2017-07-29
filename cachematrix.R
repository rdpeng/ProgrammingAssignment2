## Pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set,          
       get = get,          
       setinverse = setinverse,  
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"

cachesolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrice <- x$get()
  i <- solve(matrice, ...)
  x$setinverse(i)
  return(i)
}

## Example
myMatrix <- makeCacheMatrix(matrix(c(3, 5, 7, 9), 2, 2))
myMatrix$get()
myMatrix$getinverse()
cachesolve(myMatrix)
cachesolve(myMatrix)
