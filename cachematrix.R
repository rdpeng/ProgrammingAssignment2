#This function creates a special "matrix" object that can cache its inverse. It returns an object of type "list". 

makeCacheMatrix <- function(x = matrix()) {   #Object x initialization as an empty matrix in the function argument list
  inv_x <- NULL                               #Object inv_x initialization to NULL
  set <- function(y) {   # Function set 
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv_x <<- solve
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv_x <- x$getinv()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setinv(inv_x)
  inv_x
}
