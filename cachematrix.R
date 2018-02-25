## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Write a short comment describing this function
## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
      message("inverse is cached")
      return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinv(inv)
    return(inv)
  }

