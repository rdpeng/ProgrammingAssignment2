##The functions created here are used to create a special object that stores a matrix and caches its inverse.
##The first function, makeCacheMatrix creates a special “matrix”
##which is a list containing a function to:

## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse

## Assume the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(b) {
    x <<- b
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve should retrieve the inverse from the cache.

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
