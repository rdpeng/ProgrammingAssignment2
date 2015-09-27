## makeCacheMatix is a function whose input is square matrix.
## The output is a list of four subfunctions:
## set: allows to set a matrix
## get: returns the matrix
## setinv: sets the value of the inverse matrix
## getinv: returns the value of the inverse matrixx

makeCacheMatrix <- function(x = matrix()) 
{
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



## CacheSolve returns the cached value of the inverse of a matrix
## The calculation is performed only once (first time), 
## Therefore it avoids recalculation by storing the value in memory


cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv                 ## Return a matrix that is the inverse of 'x'
}
