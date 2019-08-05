## Put comments here that give an overall description of what your
## functions do
#A pair of functions that cache the inverse of a matrix
## Write a short comment describing this function
#Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<<-NULL
  set<-fuction(y){
    x<<-matrix()
    m<<-NULL
  }
  get<-function() x
  setInserve<-function(inserve){
    m<<-inserve
  }
  getInserve<-function() m
  list(set = set, get = get,
       setInserve = setInverse,
       getInserve = getInserve)
}

## Write a short comment describing this function
#Compute the inverse of the special matrix returned by "makeCacheMatrix" above.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m     
  ## Return a matrix that is the inverse of 'x'
}
