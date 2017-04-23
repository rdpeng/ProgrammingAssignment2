
## Coursera: R Programming Week 3 Assignment 
## User: Yueying Zhang


# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver < - NULL                          # initialize the inverse matrix as Null
  set <- function(y) {                    # define the set function
    x <<- y
    inver <<- NULL
  }
  get <- function() x                     # define the get function
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  dat <- x$get()
  inver <- solve(dat, ...)
  x$setinverse(inver)
  inver             
  ## Return a matrix that is the inverse of 'x'
}

