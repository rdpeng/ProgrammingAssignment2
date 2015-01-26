## This function creates a special "matrix" object that can cache its inverse.


#The first function, we create a special matrix, to
#set the value of the matrix
#get the value of the matri
#set the value of the inverse square
#get the value of the inverse square

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

#It first checks to see if the inverse square of the matrix has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse square of the matrix called "data" and sets the value 
#in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}