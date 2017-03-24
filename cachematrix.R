## MakeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if (!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  myData <- x$get()
  inverseMatrix <- solve(myData, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}

## Test Requirements for the above functions

newMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))

newMatrix$get()                                                   ## Returns matrix

newMatrix$getInverse()                                            ## Returns matrix inverse

cacheSolve(newMatrix)                                             ## Computes, caches, and returns new matrix inverse

cacheSolve(newMatrix)

newMatrix$set(matrix(c(1, 5, 4, 2), nrow=2, ncol=2))               ## Modify existing matrix

newMatrix$get()                                                    ## Returns matrix

newMatrix$getInverse()                                             ## Returns matrix inverse

cacheSolve(newMatrix)                                              ## Computes, caches, and returns new matrix inverse

newMatrix$getInverse()                                             ## Returns matrix inverse

cacheSolve(newMatrix)