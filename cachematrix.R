## The functions will seek to find the inverse of a given matrix. If it exists from
## a prior entry, it will use a cache instead of finding it again.

## Function will ask for matrix info and will give inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## Here the function will set to find the inverse of the matrix. If it has not
## been find prior, it will solve it. If it has already been solved, it will pull
## from the cache instead of solving. 
  
  cacheSolve <- function(x, ...){
    invMatrix <- x$getInverse()
    if(!is.null(invMatrix)){
      message("getting cached data")
      return(invMatrix)
    }
    data <- x$getMatrix()
    invMatrix <- solve(data, ...)
    x$setInverse(invMatrix)
    return(invMatrix)
  }
