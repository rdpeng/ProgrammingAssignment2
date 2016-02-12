## Computing matrix inversion is a system intensive task. We can optimize it by  
## caching the inverse of a matrix when it is computed for the first time rather 
## than computing it again and again if the matrix is not changed.

## This function creates special matrix object and returns a list of functions which 
## are as follows:
## set(y) -> Use it to set matrix value.
## get()  -> Use it to get matrix value. 
## setInverse(inverse)  -> Use it to set inverse of matrix.
## getInverse()  -> Use it to get inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invx <<- inverse
  getInverse <- function() invx
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function calculates the inverse of a special matrix object created by using 
## makeCacheMatrix function. When called it will first check if inverse has already 
## been calculated. If not then it will calculate the inverse of a given matrix   
## and cache it so that the next time when function is invoked again for the 
## same matrix it will return inverse from cache instead of calculating it again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mtrx <- x$get()
  invx <- x$getInverse()
  if(all(mtrx %*% invx == diag(nrow(mtrx)))) {
    message("getting cached data")
    return(invx)
  }
  invx <- solve(mtrx, ...)
  x$setInverse(invx)
  invx
}
