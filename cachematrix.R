# The first function 'makeCacheMatrix' just creates a matrix which
# is then in the second function 'cacheSolve' used to calculate
# the inverse of that matrix. 

# This first function creates a matrix 'mat'. It will set
# the value of the matrix and get the value of the matrix.
# Then it will set the value of the inverse of the matrix
# and get the value of the inverse of the matrix.

makeCacheMatrix <- function(mat = matrix()) {
      inv <- NULL
      set <- function(y) {
            mat <<- y
            inv <<- NULL
      }
      get <- function() mat
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


# This function will return the inverse of the matrix,
# which was created with the first function.
# If the cached inverse of the matrix is already available,
# it will return the inverse from the cache and skip the 
# computation process.
# If not, then it calculates the inverse of the matrix, caches
# and returns it.

cacheSolve <- function(x, ...) {
      inv <- mat$getinverse()
      if(!is.null(inv)) {
            message("Getting cached data!")
            return(inv)
      }
      data <- mat$get()
      inv <- solve(data, ...)
      mat$setinv(inv)
      inv
}
