## Put comments here that give an overall description of what your
## functions do

## The following function has several functions with in
## that Get

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  #sets empty variable
  
  set <- function(y) { #Puts matrix into x
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
 
   x$setinverse(i)
  
  return(i)
  }
