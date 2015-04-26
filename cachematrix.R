## Functions are used for making matrix with cache for its inverse, and
## making inversion with ability to set & get it in / from cache, without redundant
## computations when repeatedly making inverse on same data.

## This function takes matrix as input, creates a special "matrix" object
## that can cache it's inverse and return cached matrix.
makeCacheMatrix <- function(m = matrix()) {
  
  # initialize cache for inversion for first time
  inv_cache <- NULL
  
  # set new matrix data & reset cache
  set <- function(m2 = matrix()) {
    m <<- m2
    inv_cache <<- NULL    
  }  
  # get matrix data
  get <- function() {m}
  
  # set matrix inversion to cache
  setinv <- function(inv_m) {inv_cache <<- inv_m}
  # get inversion from cache
  getinv <- function() {inv_cache}
  
  # return list of pseudo-methods for created matrix 
  list(set = set, get=get, setinv = setinv, getinv=getinv)
}

## This function takes special "matrix" returned by makeCacheMatrix as input
## & computes it's inverse for retrieval. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(m, ...) {
  
  # get & check inverse from cache
  inv_m <- m$getinv()
  if (!is.null(inv_m)) {
    message("Getting cached data...")
    # return inverse if already calculated and set to cache
    return(inv_m)
  }
  
  # otherwise - get matrix data, calculate inverse & set it to cache
  data <- m$get()
  # print error message (tryCatch()), if there is an error 
  # associated with division by zero while make inverse
  inv_m <- tryCatch(solve(data), error = function(err){message("Matrix hasn't an inverse due division by zero. Check values of matrix.")})
  m$setinv(inv_m)
  # return inverse
  inv_m
}