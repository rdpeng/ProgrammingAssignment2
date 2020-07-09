## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
 
   set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv_matrix <<- inverse
  getinv <- function() inv_matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinv()
  if(!is.null(inv_matrix)) {
      message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setinv(inv_matrix)
  inv_matrix
}

# Matrix to test the functions

x <- matrix(1:4, 2, 2)
  cache_matrix <- makeCacheMatrix(x) 

cacheSolve(cache_matrix)
## solution ##
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
