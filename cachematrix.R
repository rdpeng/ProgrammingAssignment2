## The following function(s) demonstrates a list of what do the
## functions of what a matrix do
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

## This function below is to create a special object to store a numeric 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function returns the result of the inverted matrix.
## It would first check to see if the matrix is already been solved.
## If not, it calculates the the inverse of the data and sets the value
## of the cache via the setinverse function. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}

mat <- matrix(data = c(9,12,7,5), nrow = 2, ncol = 2)
mat2 <- makeCacheMatrix(mat)
cacheSolve <- function(x, cachematrix)
  mat2
mat
