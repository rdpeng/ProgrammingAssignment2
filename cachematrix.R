## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
  
makeCacheMatrix <- function(x = matrix()) {
  ##default value
  inv <- NULL
  ## function to set x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## return x the matrix
  get <- function() x
  ## overwrite x
  setinv <- function(m) inv <<- m 
  ## return the cached inverse (may be null)
  getinv <- function() inv
  ## return function handles
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##check the cache
  inv <- x[["getinv"]]()##get error with the $ operator
  if(!is.null(inv)) {
    message("getting cached data");
    return(inv)
  }
  ##if cache is null, calculate the inverse
  ##and save in the cache
  data <- x[["get"]]()
  inv <- solve(data, ...)
  x[["setinv"]](inv)
  ##return the result
  inv
}
