## makeCacheMatrix -> Create an object that can store a matrix and its inverse
## cacheSolve -> Return the inverse of the matrix, if not calculated yet then solves and stores the data

## Create the matrix with the methods to set/get the inverse

makeCacheMatrix <- function(x = matrix()) {
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


## Return the inverse either by retrieving the stored data or by calculating it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## Check if the value has been already calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Solves and stores the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
