## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x <<- y
    mat <- NULL
  }
  get <- function() x
  set_inv <- function(inverse) mat <<- inverse
  get_inv <- function() mat

  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  mat <- x$get_inv()
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$set_inv(mat)
  mat
        ## Return a matrix that is the inverse of 'x'
}
