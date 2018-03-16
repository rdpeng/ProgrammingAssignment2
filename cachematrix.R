## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  #set the value of the matrix
  set <- function(mat) {
    x <<- mat;
    inverse <<- NULL;
  }
  
  #get the value of the matrix
  get <- function() x;
  
  #set the value of the inverse matrix
  setinv <- function(inv) inverse <<- inv;
  
  #get the value of the inverse matrix
  getinv <- function() inverse;
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #get the value of the inverse matrix
  invMat <- x$getinv()
  
  #get the cached value of the inverse matrix
  if(!is.null(invMat)) {
    message("getting cached data...")
    invMat
  }
  
  #set the value of the inverse matrix
  data <- x$get()
  invMat <- solve(data)
  x$setinv(invMat)
  
  #return inverse matrix
  invMat
  
 
}
