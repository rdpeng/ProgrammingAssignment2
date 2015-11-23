## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse
## assume the matrix supplied is always invertible
## a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## using <<- to assign a value to an object in a different envoirnment
  set <- function(y) {
    x <<- y     
    m <<- NULL 
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix().
## If the inverse has already been calculated and the matrix has not changed,
## it retrieves the inverse from the existing cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("please hold while I get me some cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
