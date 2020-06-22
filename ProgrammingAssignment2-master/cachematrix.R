## I have created an array called makeCacheMatrix whose 
##objective is to get its inverse.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse)j <<- inverse
  getInverse <- function()j
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##With the following function I can find the inverse of the matrix 
##that is created in the previous function

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat, ...)
  x$setInverse(j)
  j
}
