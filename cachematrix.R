## Week 3 Assignment: Functions that cache the reverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #initiate
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  # set inverse of matrix
  setinverse <- function(inverse) m <<- inverse
  
  #get inverse of matrix
  getinverse <- function() m
  
  #return function list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  #check if matrix is cached
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #inverse matrix if not cached
  data <- x$get()
  m <- solve(data)  %*% data
  x$setinverse(m)
  
  #return matrix
  m
}