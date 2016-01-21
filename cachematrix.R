## Put comments here that give an overall description of what your
## functions do

## this function 'makeCacheMatrix' generates a set of 4 functions
## that perform following functions
## "set" --> takes a matrix input, on which inverse 
##can computed in future
## "get" --> looks up, if matrix has already been assigned in that object
## "setinv" --> receives and stores the 'inverse'
## "getinv" -> outputs the stored 'inverse'

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function expects to recieve the output list from an
## object of the function 'makeCacheMatrix'
## if inverse has already been computed before it will retrive that
## else it will compute it using the solve()function.
cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Providing inverse from cache..")
    return(inv)
    
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}
##