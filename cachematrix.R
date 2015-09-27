## Put comments here that give an overall description of what you functions dor


# The function "makeCacheMatrix" creates the inverted matrix save area which is
# updated by using the superassignment operator "<<-".

makeCacheMatrix <- function(x = matrix()) {

        matrix_cache <- NULL            # setMatrix() inversion result storage area

        # Create the matrix in the global environment
        set <- function(y) {
                x <<- y
                matrix_cache <<- NULL   # set / reset storage area 
        } # end set

        # Get the value of the matrix
        get <- function() x

        # Store the inverted matrix in cache
        setInverse <- function(inverse) matrix_cache <<- inverse

        # Get the inverted matrix from cache
        getInverse <- function() matrix_cache

        # Return a list of functions to the working environment
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
             
}  # end makeCacheMatrix


## Write a short comment describing this function

# The function "cacheSolve" retrieves the inverted matrix (if present). If the return
# value is NULL, the matrix will be inverted (vi solve()) and stored in the cache.

cacheSolve <- function(x, ...) {

      im <- x$getInverse()          # Get the inverted matrix from x 

      if(!is.null(im)) {            # If an inverted matrix has been stored ...
	  message("Found cached data")
	  return(im)                # ... return the stored inversion
      }  # end if()

      mat <- x$get()                # If not, use x$get to get the matrix 
      im <- solve(mat, ...)         # Solve (invert) it
      x$setInverse(im)              # Save it
      return(im)                    # Return the solved result
	
}  # end cacheSolve()
