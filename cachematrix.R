## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    orig_matrix <<- y
    inv_matrix <<- NULL
  }
  
  get <- function() orig_matrix
  
  setinv <- function(solve) inv_matrix <<- solve
  getinv <- function() inv_matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- orig_matrix$getinv()
  if(!is.null(inv_matrix)){
    message("getting the cached inverse")
    return(inv_matrix)
  }
  data<-orig_matrix$get()
  inv_matrix<-source(data, ...)
  orig_matrix$setinv(inv_matrix)
  inv_matrix
  
}
