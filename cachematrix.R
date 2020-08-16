##Inversion of Matrix is a costly operation. The following function helps us cache the inverse of matrix.
##If the Inverse for that Matrix has been already calculated it will fetch the inverse from cache.
##Assuming that the matrix that is entered is invertible.

##  makeCacheMatrix() creates the matrix object , sets the value of Matrix, gets the Value of Matrix, sets the Value of inverse and gets the Value of Inverse.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() computes the inverse of the Matrix using solve(). If the inverse has already been computed in makeCacheMatrix() it retrieves it from cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Inverse computed. Getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
