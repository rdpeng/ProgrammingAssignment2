## These functions create a special matrix object and cache
## its inverse in order to decrease time needed for computation

## This function creates a special matrix object and cache's
## its inverse. A list is returned with the functions needed to
## set the value of the matrix, get the value of the matrix,
## set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversematrix <- NULL
  set <- function(y){
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversematrix <<- inverse
  getinverse <- function() inversematrix
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## This function retrieves the inverse of a given matrix from
## the cache if it exists or calculates the inverse and
## cache's it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)){
    message("getting cached data")
    return(inversematrix)
  }
  message("calculating inverse")
  data <- x$get()
  inversematrix <- solve(data)
  x$setinverse(inversematrix)
  inversematrix
}
