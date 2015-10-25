## overall description of the functions
# Matrix inversion is usually a costly computation.
# There may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
# Below are two functions that cache the inverse of a matrix.



##  function description
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# The special "matrix" is really a list containing a function to:
# set the values of the matrix
# get the values of the matrix
# set the values of the Inverse matrix
# get the values of the Inverse matrix
makeCacheMatrix <- function(The.Matrix = matrix()) {
      Inv.Matrix <- NULL
      
      set <- function(New.Matrix) {
            The.Matrix <<- New.Matrix
            Inv.Matrix <<- NULL
      }
      get <- function() The.Matrix
      setInv <- function(Invert) Inv.Matrix <<- Invert
      getInv <- function() Inv.Matrix
      
      list(
           set = set,
           get = get,
           setInv = setInv,
           getInv = getInv
      )
}


##  function description
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      CheckInv <- x$getInv()
      if(!is.null(CheckInv)) {
            message("getting cached data")
            return(CheckInv)
      }
      The.Matrix <- x$get()
      New.Inv <- solve(The.Matrix)
      x$setInv(New.Inv)
      New.Inv
}
