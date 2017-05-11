## Put comments here that give an overall description of what your
## functions do
## The first function is turning a matrix to a special one, then by using the second function, 
## we can calculate and save the inverse of that matrix, so that R do not calculate it over and over.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## This function is the one creating a special matrix.
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function
## This function below is caching the inverse of matrix "x".
cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("cache")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
