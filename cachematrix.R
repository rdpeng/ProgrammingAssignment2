## Assignment: Caching the Inverse of a Matrix
## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the Inverse
## 4. get the value of the Inverse

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  # Set the value of the Matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Get the value of the Matrix
  get <- function() x
  
  # Set the value of Inverse of the Matrix
  setInverse <- function(inverse) i <<- inverse
  
  # Get the value of the Inverse of the Matrix
  getInverse <- function() i
  list(set=set, get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
  
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Call the getInverse function defined in the makeCacheMatrix function  
  i <- x$getInverse()
  
  #Check if present in cache
  # If the variable is null, not in cache
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
