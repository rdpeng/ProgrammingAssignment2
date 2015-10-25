## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
  #creates a special matrix object that can cache its inverse
  {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {  #if inv is already compute, then write a message 
    #and return the computed inversion
    message("getting cached data.")
    return(inv)
  }
  data <- x$get() #else compute the inversion
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
