## Put comments here that give an overall description of what your
## functions do
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

## Write a short comment describing this function
##makeCacheMatrix  function creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   j <- NULL
  set <- function(y){     ##set the value of the vector
  x <<- y
  j <<- NULL
  }
  get <- function()x  ##get the value of the vector
  setInverse <- function(inverse) j <<- inverse   ##set the value of the mean
  getInverse <- function() j     ##get the value of the mean
  list(set = set, get = get, 
  setInverse = setInverse,   ## Produced with the list 
  getInverse = getInverse)

}


## Write a short comment describing this function
##This function computes the inverse of the special “matrix” returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   j <- x$getInverse()      ##checks If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()  #gets the mat
  j <- solve(mat,...) ##convert it to inverse.
  x$setInverse(j)
  j
}
