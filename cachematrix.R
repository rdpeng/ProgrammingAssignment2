## Put comments here that give an overall description of what your
## functions do

# Lexical Scoping 2nd try

makeCacheMatrix <- function(x = matrix()) {
  # create a special matrix calculate the inverse of a matrix
m <- NULL
set <- function(inverse) {
  x <<- inverse
  m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)) {return(m)}
  ## Return a matrix that is the inverse of 'x' if inverse already exist
  data <- x$get()
  m <- inverse (data,...)
  x$setinverse (m)
  m

}
