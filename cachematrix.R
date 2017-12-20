## This makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## which is really in a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # apply set to modify X and reset the inv to Null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # apply get to retrieve the stored matrix
  get <- function() x
  
  # apply setinverse to store the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse 
  
  # apply getinverse to return the stored inverted matrix.
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # find out if a stored value for the inverted matrix exists, 
  # return the inverted matrix if it exists
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # if inv is null, solve the inverse of x and cache the result
  matrix_data <- x$get()
  inv <- solve(matrix_data, ...)
  x$setinverse(inv)
  inv
}
        
