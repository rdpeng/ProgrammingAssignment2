# The following functions store a matrix and solve this matrices inverse.
# The matrix and its inverse can be accessed and changed. If the inverse
# is calculated it is stored in the cache. When any matrix is passed to 
# the function to calculate its inverse, the function will check to see if 
# the inverse matrix is in the cache. If it is in the cache it will be returned
# as is. If it is not in the cache the inverse will be calculated and returned.

## makeCacheMatrix
	# Takes a square matrix as its input.
	# Returns a list of functions to:
		# access the matrix.
		# set (change) the matrix.
		# access the matrices inverse.
		# set (change) the matrices inverse.

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
         x <<- y
         inverse <<- NULL
      }
      get <- function() x
      set_inv <- function(inv)  inverse <<- inv
      get_inv <- function() inverse
      list(set=set, get=get, set_inv=set_inv, 
           get_inv=get_inv)
}


## cacheSolve
	# Takes the list of functions from makeCacheMatrix as input.
	# Checks if inverse of matrix exists in cache.
	# If inverse of matrix exists in cache, it will be returned.
	# If inverse of matrix does not exist in cache, it will be 
	# calculated and returned.

cacheSolve <- function(x, ...) {
   inverse <- x$get_inv()
   
   if (!is.null(inverse)){
      message("Getting Data from Cache")
      return(inverse)
   }
   else{
	   mat_data <- x$get()
	   inverse <- solve(mat_data, ...)
	   x$set_inv(inverse)
	   return(inverse)
   }
        ## Return a matrix that is the inverse of 'x'
}
