## makeCacheMatrix creates a special matrix object, which allows 
##cacheSolve to take cached inverse value if it was calculated before
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
    x<<- y
    inv<<- NULL
  }
  get <- function() x
  setneg <- function(solve) inv<<- solve
  getneg <- function() inv
  list(set = set, get = get,
       setneg = setneg,
       getneg = getneg)
}

## Function returns inverse matrix using solve() function, but if cached inverse value is available
## then it takes it.

cacheSolve <- function(x, ...) {
  inv <- x$getneg()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setneg (inv)
  inv
}
