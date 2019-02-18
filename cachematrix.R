# FILE:
#   cachematrix.R
#
# DESCRIPTION:
#   Two functions that maintains a matrix and its inverse.
#
# FUNCTIONS:
#   makeCacheMatrix(as.matrix(A))
#     Returns a list of functions
#       list(set_matrix(as.matrix(B)), 
#            get_matrix(),
#            set_inverse(as.matrix(Binv)), 
#            get_inverse())
#   cachesolve(as.makeCacheMatrix(x))
#     Returns the inverse of the matrix
#       The first time the function is called the inverse is computed
#       and stored for later use.
#
# AUTHOR:
#   Andreas Atle, atle.andreas@gmail.com
#

# FUNCTION: makeCacheMatrix - create four functions
# RETURNS: list(set_matrix,get_matrix,set_inverse,get_inverse)
makeCacheMatrix <- function(A = matrix()) {
  message("Make a new makeCacheMatrix")

  # Initialize inverse to NULL
  Ainv <- NULL
  
  # Set matrix A to new matrix B
  set_matrix <- function(B) {
    A <<- B
    Ainv <<- NULL
  }

  # Get matrix A
  get_matrix <- function() {
    A
  }
  
  # Set inverse matrix
  set_inverse <- function(Binv) {
    Ainv <<- Binv
  }
  
  # Get inverse matrix
  get_inverse <- function() {
    Ainv
  }
  
  # Return list of functions
  list(set_matrix = set_matrix,
       get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# FUNCTION: cachesolve - retrieve the inverse matrix
# RETURNS: inverse matrix
cachesolve <- function(x, ...) {
  message("Call cachesolve")
  
  # Retrieve the inverse from the "CacheMatrix"
  Ainv <- x$get_inverse()

  if(is.null(Ainv)) {

    # Compute and set the inverse in the "CacheMatrix"
    data <- x$get_matrix()
    Ainv <- solve(data, ...)
    x$set_inverse(Ainv)
    message_str <- "Compute new inverse"
  
  } else {
  
    # Use the cached inverse from the "CacheMatrix"
    message_str <- "Use cached inverse"

  }
  
  # Write a message if computed or cached
  message(message_str)

  # Return the inverse
  Ainv
}
