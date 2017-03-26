## makeCacheMatrix function creates a list containing function for
##setting matrix, getting the matrix, setting inverse and getting inverse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(z) i<<-z
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve first checks if inverse has already been computed.If so, it returns the inverse from cache.
##If not,it calculates the inverse and sets the inverse value in the cache.

cacheSolve <- function(x, ...) {
       i<-x$getinverse()
  if(!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
