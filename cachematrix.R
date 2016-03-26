## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the Inverse matrix
  m <- NULL
  ## set matrix
   set <- function(y){
    x<<-y
    m<<-NULL
  }
  ## get matrix
  get <- function() x
  ## set Inverse of the matrix
  setInvMat <- function(Inverse) m <<- Inverse
  ## get Inverse of the matrix
  getInvMat <- function() m
  ##Return a list of the processes facts
  list(set=set, get=get,
            setInvMat=setInvMat,
            getInvMat=getInvmat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  m <- m$getInvMat()
  ## Return the inverse if its already stored
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## If not
  ## get the matrix
  data <- x$get()
  ## Use matrix multiplication to get the inverse
  m <- solve(data, ...) %*% data
  ## Set the Inverse
  x$setInvMat(m)
  m
}
