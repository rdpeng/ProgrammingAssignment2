## The functions "makeCacheMatrix" and "cacheSolve" are created to cache the inverse of a matrix
# to reduce the computation time of Matrix Inversion

## Under the function "makeCacheMatrix", an inverse matrix is stored inside the object inv.
# The output of the function is a list with 4 named elements, which are 
# the four functions defined as: setMatrix, getMatrix, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y){     ## set the matrix value
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x     ## getting the matrix value cached with setmatrix
  setInverse <- function(solve) inv <<- solve   ## cached value of inverse matrix is saved in inv
  getInverse <- function() inv  ## getting the cached value of inverse matrix saved in inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## The function "cacheSolve" computes the inverse of the special matrix returned by "makeCacheMatrix" function above.
# If the inverse has already been calculated (and the matrix has not changed), then the "cacheSolve" function should retrieve the inversed matrix from the cache
# and send a message before giving out the inversed matrix.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()         ## if an inverse has already been calculated, inversed matrix is retrieved
  if(!is.null(inv)){                   ## checking if the matrix has been run before
    message("getting inversed matrix") ## if so, a message will come out and value of inversed matrix is returned
    return(inv)
  }
  matrix <- x$getMatrix()       ## however if the matrix has not been calculated before, the matrix will be solved
  inv <- solve(matrix, ...)     
  x$setInverse(inv)             ## run the setInverse function on the inverse matrix to cache it
  inv                           ## return the inverse matrix
}
