## Function that chaches the inverse of a matrix
## 
## Create special matrix, of a list containing a function to:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInv <- function(inverse)inv <<- inverse
  getInv <- function()inv
  list(
    set = set,
    get = get,
    setInv = setInv,
    getInv = getInv
  )
}
##
## Calculate inverse of matrix created in above function, reuse cached result if available
##
cacheSolve <- function(x, ...){
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
    }
  m <- x$get()
  inv <- solve(m, ...)
  x$setInv(inv)
  inv
}