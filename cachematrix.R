##  This function creates a special "matrix" object 
##  that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInversion <- function(inversion) i <<- inversion
  getInversion <- function() i
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion)
}


## This function computes the inverse of the special "matrix".
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  i <- x$getInversion()
  if(!is.null(i)) {
    message("getting cached inversion of matrix ")
    return(i)
  }
  mat <- x$get()
  d <- det(mat)
  if (d == 0 ) {
    print ("Determinant is 0, Inversion of Matrix don't exists !!!")
  } else {
    
    i <- solve(mat, ...)
    x$setInversion(i)
    i
  }
}
