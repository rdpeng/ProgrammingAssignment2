## Put comments here that give an overall description of what your
## functions do

## this function creates a special "matrix", that store inverse of matrix in a "cache" variable, 
## so it could be retrieved with no need for comuptation 

makeCacheMatrix <- function(x = matrix()) {
          inv_mat <- NULL
  set <- function(y) {
    mat <<- y
    inv_mat <<- NULL
  }
  get <- function() mat
  setinv <- function(inv) inv_mat <<- inv
  getinv <- function() inv_mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## this function calculates the inverse of matrix in makeCacheMatrix object,  
## However, it first checks to see if the inverse of matrix has already been calculated.
## If so, it gets the inverse of matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of matrix and sets the value of the inverse of matrix in the cache via the setinv function.
## if inverse matrix is cached, cached inverse of matrix is returned 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         
  imv_mat <- x$getinv()
  if(!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setinv(inv_mat)
  inv_mat
}
