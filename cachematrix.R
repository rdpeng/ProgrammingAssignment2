## These functions create a special matrix object and cache
## its inverse in order to decrease time needed for computation

## This function creates a special matrix object and cache's
## its inverse. A list is returned with the functions needed to
## set the value of the matrix, get the value of the matrix,
## set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize matrix
  inversematrix <- NULL
  
  ## set the value of the matrix
  set <- function(y){
    x <<- y
    inversematrix <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse
  setinverse <- function(inverse) inversematrix <<- inverse
  
  ## get the value of the inverse
  getinverse <- function() inversematrix
  
  ## create list of all functions above
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## This function retrieves the inverse of a given matrix from
## the cache if it exists or calculates the inverse and
## cache's it.

cacheSolve <- function(x, ...) {
  ## get the value of the inverse
  inversematrix <- x$getinverse()
  
  ## if the inverse has been calculated, get the value from cache
  if(!is.null(inversematrix)){
    message("getting cached data")
    return(inversematrix)
  }
  
  ## otherwise calculate the inverse and save it to cache
  message("calculating inverse")
  data <- x$get()
  inversematrix <- solve(data)
  x$setinverse(inversematrix)
  inversematrix
}
