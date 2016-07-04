## This function creates a special matrix and caches it's inverse

## Create a cache matrix (a copy of the original matrix)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
   x <<- y
   i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Calculate and cache the inverse of the special matrix
## New Branch Rbranch
cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)){
    message("Getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(x)
  x$setInv(i)
  i
}
