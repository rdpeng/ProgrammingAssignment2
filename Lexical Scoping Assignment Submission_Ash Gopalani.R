## This function is to create a special matrix object that can cache its inverse

makeCachematrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function()j
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function will compute the inverse of the matrix from makeCachematrix

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
