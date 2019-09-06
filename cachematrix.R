## These 2 functions return the inverse matrix that is given.
## If the inverse of the matrix is already calculated then these function will not calculate it again and they will return the cached data
## makeCacheMatrix is where you give the matrix that want to be inversed and casheSolve is
## where the inverse matrix is calculated or the cached data is returned
##  "MatInverse" is the inverse of x. 
## Function setInverse sets the inverse of the matrix and function getInverse returns the inverse matrix
## in cacheSolve, the function first looks up for the calculated inverse matrix and if it exists,it will show it
## but if not, it will calculate it


makeCacheMatrix <- function(x = matrix()) {
  MatInverse<- NULL
  set <- function(y) {
    x <<- y
    MatInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) MatInverse <<- inverse
  getInverse <- function() MatInverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
  MatInverse <- x$getInverse()
  if (!is.null(MatInverse)) {
    message("getting cached data")
    return(MatInverse)
  }
  Matrix <- x$get()
  MatInverse <- solve(Matrix, ...)
  x$setInverse(MatInverse)
  MatInverse
}
