## These functions look in the cache to see whether an inverse matrix for a
## particular matrix has already been calculated and if so, uses this matrix rather
## than recalcuate a new one each time that would be more 'expensive'.

## This first function initialises an anonymous matrix in the parent environment that
## will ultimatley allow it to store pre-configured solutions in the cache for easy access

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solveinverse) inv <<- solveinverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function then looks at whether the relevant inverse matrix exists in the
## cache and if not, calculates a new inverse matrix and stores this in the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

testmatrix<-matrix(c(1,4,3,0,-3,2,7,7,8),3,3)
functest <- makeCacheMatrix(testmatrix)
cacheSolve(functest)
