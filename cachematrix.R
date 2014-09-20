## This function will create a list of functions that can cache the inverse of a matrix
## as asked by the assignment

makeCacheMatrix <- function(x = matrix()) {
  ## setting the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## getting the value of the matrix
  get <- function() x
  ## setting the value of the inverse of the matrix
  setInverse <- function(inverse) m <<- inverse
  ## getting the value of the inverse of the matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function will compute the inverse of the matrix
## returned by the function above. If the inverse 
## was already computed, it will retrive it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## checking to see if the inverse was already computed
  if (! is.null(m)) {
    print ("getting data in the cache")
    return(m)
  }
  ## computing the inverse of the matrix if needed
  m <- solve(x$get())
  x$setInverse(m)
  m
}



