## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## cacheSolve function function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

##Creates a special matrix and returns a list of set and get the value of the matrix,
##set and get the value of Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <-function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Returns the inverse of the special matrix created with makeCacheMatrix function 
## First checks whether the inverse of the matrix has already been calculated
## If yes, it gets the mean from the cache and skips the computation

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <-x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}