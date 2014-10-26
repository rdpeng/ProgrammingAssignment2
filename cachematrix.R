## Programming Assignment 2: Lexical scoping

## Taking inverse of matrix repeatedly is usually a costly computation.

## Caching the inverse of a matrix may be beneficial rather than computing it repeatedly.

## The "makeCacheMatrix" function creates a special matrix object, 
## that can be used to cache it's inverse.

## The "cacheSolve" function computes the inverse of the matrix,
## which was defined in "makeCacheMatrix" function. 

## If the inverse has already been calculated and the matrix has not changed, 
## it retrieves the result from cache memory and gives an appropriate message.

makeCacheMatrix <- function(x = matrix()){
  
  m <- NULL
  setm <- function(y){
    x <<- y
    m <<- NULL
  }
  getm <- function() x
  setinv <- function(z) 
    m <<- z
  getinv <- function() m
  list(setmatrix = setm, getmatrix = getm, getinv = getinv, setinv = setinv)
}

cacheSolve <- function(x,...){
  m <- x$getinv()
  if(!is.null(m)){
    print("Returning cached data")
    return(m)
  } 
  m <- solve(x$getmatrix())
  x$setinv(m)
  m
}
