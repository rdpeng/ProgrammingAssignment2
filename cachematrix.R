## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This finction creats a matrix and obtains its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve)m <<- solve  #calculates inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse =getinverse)
}


## Write a short comment describing this function


##checking for cahed value and and calculating the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()           #getting the cahed data  
  if(!is.null(m)) {
    message("getting cached matricx inversion")
    return(m)
  }
  data <- x$get()
  m <- solve(data)      ##calculating the inverse
  x$setinverse(m)       ##storing the inverse
  return(m)     ## Return a matrix that is the inverse of 'x'
}
