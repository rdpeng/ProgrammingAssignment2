# Basically this is just a copy and paste of the example
# functions makeVector and cachemean, so the changes produced are minimal.

# makeCacheMatrix will produce a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL                            # First we set the inverse as null.
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x                      # This simply stores the input matrix.
  setinv <- function(new_inv) {              # setinv will store the new inverse
    inv_x <<- new_inv
  }
  getinv <- function() inv_x               # and getinv will give us the inverse
  list(set = set, get = get,                 # Finally we get the list.
       setinv = setinv, getinv = getinv)
}


# The argument x is the matrix list, which is
# the list of functions computed in the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inv_mat <- x$getinv()             # Here we get the inverse stored in the list.
  if(!is.null(inv_mat)) {
    message("getting cached inverse")
    return(inv_mat)                  # If the inverse is cached we don't need to calculate
  }                                  # that and we return the one in the list.
  data <- x$get()                   # Else we get the input matrix,
  inv_mat <- solve(data, ...)        # calculate its inverse,
  x$setinv(inv_mat)                 # and store it into the "matrix list".
  inv_mat                            # Finally we return the inverse

# Thanks a lot to Daniele Pigni and its really really useful post
# about this assignment https://class.coursera.org/rprog-013/forum/thread?thread_id=694
}
