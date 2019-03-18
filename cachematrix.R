## These functions allow us to create a cached version of an inversed ("solved") matrix, 
## and to retrieve it in case the cache exists


## This function sets the value of the matrix and gets the value of the inverse  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolved <- function(solved) m <<- solved
  getsolved <- function() m
  list(set=set, get=get, setsolved=setsolved, getsolved=getsolved)
}


## This function gets the cached data if it exists and solves the data

cacheSolve <- function(x, ...) {
  m <- x$getsolved()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolved(m)
  m}