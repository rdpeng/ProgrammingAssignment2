## Put comments here that give an overall description of what your
## functions do

# Function 1: makeCacheMatrix
# makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # inv stores cached inverse matrix
  inv <- NULL
  
  # Set
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get
  get <- function() x
  
  # Set and Get Inverse
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # Return the new matrix
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# Function 2: cacheSolve
# This function calculates the inverse of the special "matrix" returned by above function 
# If the inverse has already been calculated (and the matrix has not changed), then this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  # Check if the inverse is calculated. if not, then calculate it
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
    data <- x$get()
  inv <- solve(data)
  
  # Caching Inverse
  x$setinverse(inv)
  
  # Return the inverse
  inv
}