## These functions are written for the fulfillment of the Coursera
# Data Science: R Programming Week 3 assignment
# GitHub User:nyandele

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
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
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}

## Examples to test my code

my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()
my_Matrix$getinverse()
cacheSolve(my_Matrix)




































