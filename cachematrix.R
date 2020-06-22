# Created a function named makeCacheMtrix, assuming the matrix applied here is invertible
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){ #setting the value of matrix
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse){ inv <<- inverse}
  getInverse <- function() {inv} 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
#Created a function CacheSolve which converts the inverse of the matrix which has been returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(inv)
  inv
}



