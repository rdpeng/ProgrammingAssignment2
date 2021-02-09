## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a matrix containing a function to:

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of a matrix
#get the value of the inverse of a matrix

makeCacheMatrix <- function(x= matrix()){
  INV <- NULL
  set <- function (y){
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse){INV <<- inverse}
  getInverse <- function() INV
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function invert the special "matrix" created with the above function
## It first checks from the cache if the matrix has been inverted
## If so, it gets the inverted matrix from the cache and and skips the computation
## Otherwise, it inverts the matrix and sets the value of the matrix in the cache via the setInverse function.

cachesolve <- function (x, ...) {
  INV <- x$getInverse()
  if(!is.null(INV)){
    message("getting cached data")
    return(INV)
  }
  mat <-x$get()
  INV <- solve(mat, ...)
  x$setInverse(INV)
  INV
}
