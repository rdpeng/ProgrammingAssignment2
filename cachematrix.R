## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invrt <- NULL
  set <- function(y) {
    x <<- y
    invrt <<- NULL
  }
  get <- function() x
  setInverseM <- function(inverseM) invrt <<- inverseM
  getInverseM <- function() invrt
  
  list(set = set, get = get,
       setInverseM = setInverseM,
       getInverseM = getInverseM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrt <- x$getInverseM()
  if(!is.null(invrt)) {
    message("getting cached data")
    return(invrt)
  }
  data <- x$get()
  invrt <- solve(data, ...)
  x$setInverseM(invrt)
  invrt
}
