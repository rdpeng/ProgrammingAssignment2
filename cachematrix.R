## Caching - Inverse of a matrix

## Simply put, the following function creates a 'special vector', just like the
## example above where the 'special vector' is really just a list of 4 containing
## a function to: 
# 1) set the dimensions and values of the elements of matrix
# 2) get the matrix
# 3) set the value of the inverse of the matrix 
# 4) get the inverse of the matrix 



makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y 
    inv_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matrix <<- inverse
  getinverse  <- function() inv_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The following function calculates the inverse of the matrix supplied. But
## before that it checks if the inverse has been calculated (and the matrix has 
## not changed). then the function should retreive it from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  matrix <- x$get()
  inv_matrix <- solve(matrix)
  x$setinverse(inv_matrix)
  inv_matrix
}
