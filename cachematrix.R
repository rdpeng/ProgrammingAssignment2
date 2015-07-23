 
## The functions are used to create a special object that stores a 
## matrix and caches it's inverse.

## Function makeCacheMatrix is a function that allows to set or store a
## matrix 'x' and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Function cacheSolve is a function that returns the stored inverse matrix of 'x'.
## If there is nothing stored, it calculates the inverse of the given matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  message("no inverse matrix stored, calculating inverse")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
