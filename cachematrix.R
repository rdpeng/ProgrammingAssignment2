# 'makeCacheMatrix'  creates a special "matrix" object. 
# 'cacheSolve' function computes the inverse of the special "matrix" returned.
# 'makeCacheMatrix'  creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  int <- NULL
  set <- function(a) {
    dig <<- a
    int <<- NULL
  }
  get <- function() dig
  setinverse <- function(inverse) int <<- inverse
  getinverse <- function() int
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# 'cacheSolve' function computes the inverse of the special "matrix" returned by makeCacheMatrix above and 
# if the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(dig, ...) {
  int <- int$getinverse()   # Return a matrix that is the inverse of 'dig'
  if(!is.null(int)) {
    message("getting cached data")
    return(int)
  }
  else{
  data <- dig$get()
  int <- mean(data, ...)
  dig$setinverse(int)
  int
  }
}
