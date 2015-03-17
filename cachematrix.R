## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #setting inverse to NULL just for variable creation
  inverse <- NULL 
  #set function for store in cache the matrix, at start inverse is empty
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #get fuction for the stored matrix
  get <- function() x
  #setter method for the inverse matrix
  setinverse <- function(inv) inverse <<- inv
  #getter method for the invese matrix
  getinverse <- function() inverse
  #return statment fot the whole structure
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #firt it tries to retrieve the cached inverse matrix
  i <- x$getinverse()
  #if inverse matrix is not null it means that is stored so it can be returned
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  #if we reach this point the inverse matrix cached was empty
  #it could happen if is the first time we call the cacheSolve function on the cacheMatrix
  #or it could be that after the call of the method set the inverse matrix cached was dropped
  #so it retrieve the data
  data <- x$get()
  #it computes the inverse matrix
  i <- solve(data, ...)
  #it caches the inverse matrix computed
  x$setinverse(i)
  #it returns the inverse matrix for 'x' 
  i
}
