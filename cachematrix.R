## Put comments here that give an overall description of what your
## functions do

##The following function is used to create a special object that stores a matrix and caches its inverse. 
#makeCacheMatrix creates a special “matrix”, which is  a list containing a function to:
#i)set the value of the matrix,ii)get the value of the matrix,iii)set the value of the inverse
#and iv)get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



##This function computes the inverse of the  “matrix” returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
