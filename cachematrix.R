## THe first function, makeCacheMatrix,  will create a special matrix
## The second function, cacheSolve,  will cache the inverse of the matrix

## TO  create a matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse1 <- NULL
  set <- function(y)
  {
    x <<-y
    inverse<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse1 <<- inverse
  getInverse <- function() inverse1
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## To cache the inverse of the above created matrix

cacheSolve <- function(x, ...) {
  inverse1 <- x$getInverse()
  
  if(!is.null(inverse1))
  {
    return(inverse1)
  }
  
  
  mtrx <- x$get()
  inverse1 <- solve(mtrx, ...)
  x$setInverse(inverse1)
  inverse1
}
