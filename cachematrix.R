## [cache the inversed matrix of x and return it]
## First create a list of functions which set and get the inversed matrix of x then return is with the function "cachesolve"

## create a list of functions
makeCacheMatrix <- function(x = matrix()) {
  x_1 <- NULL
  set <- function(y) {
    x <<- y
    x_1 <<- NULL
  }
  get <- function() x    ### get matrix x
  rev_matrix <- function(solve) x_1 <<- solve
  get_inv_matrix <- function() x_1
  list(set = set, get = get,      ### store functions as a list
       inv_matrix = inv_matrix,
       get_inv_matrix = get_inv_matrix)
}

### this function returns the inversed matrix x_1
cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_1 <- x$get_inv_matrix()
  if(!is.null(x_1)) {
    message("getting cached inversed matrix")
    return(x_1)
  }
  data <- x$get()    ### if iniversed x is not cached
  x_1 <- solve(data, ...)
  x$inv_matrix(x_1)
  x_1
}
