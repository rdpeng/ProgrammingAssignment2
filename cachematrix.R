## makeCacheMAtrix and cacheSolve are two functions for inverting and caching the inverse only if the matrix is square invertible matrix


##makeCacheMatrix creates a special matrix object that can cache it's inverse
##It has 4 functions: 
##1-get: returns vector x stored in main function
##2-set: changes the vector stored in main function
##3-setinverse: stores the inverse of the matrix 
##4-getinverse: returns the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##sets m empty for yhe first iteration
  set <- function(y) {
    x <<- y  ##assigns the value of x from a different environment
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

  
  


## cacheSolve computes inverse of the matrix x, if already computed it calls the cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()   ##if the inverse is already calculated it gets it from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##if the inverse is not in the cache, it calcultes it from scratch
  data <- x$get()
  m <- solve(data, ...)
  
  ##sets the value of m to calcualted inverse using the setinverse function
  x$setinverse(m)
  m
  }
