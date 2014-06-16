## makeCacheMatrix() returns a list containing a function that does the following:

## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # inv will store the cached inverse matrix
  inv <- NULL
  
  # set the matrix
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the matrix
  getmatrix <- function() x
  
  # set the inverse
  setinverse <- function(inverse) inv <<- inverse
  # get the inverse
  getinverse <- function() inv
  
  # return list of 4 functions
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## The function below calculates the inverse of a matrix created in the above function.
## It first checks to see if the inverse has already been calculated.
## If yes, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the argument x (matrix)
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## returns a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # returns already calculated inverse
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # calculation of inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # cache the inverse
  x$setinv(inv)
  
  # return it
  inv
}

