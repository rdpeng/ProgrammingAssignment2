## R Programming - Week 3 - Assignment ##
#########################################

# Instructions: Caching the Inverse of a Matrix:

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
# of a matrix rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) { ## This function creates a special "matrix" object that can cache its inverse (matrix by default).                                          ## Additionally, we set the argument as matrix by default.
  inv <- NULL                               ## We start with NULL for inv, then will hold value of matrix inverse.
  set <- function(y){                       ## We define the set function, which assign new value of matrix IN THE PARENT ENVIRONMENT.
    x <<- y
    inv <<- NULL
  }
  get <- function() x                       ## It gives you back the value of the matrix argument.
  setInverse <- function(solveMatrix) inv <<- solveMatrix ## Set the inverse, inv, to solveMatrix.
  getInverse <- function() inv                            ## Returns the inverse.
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## Returns the 'special vector' containing all of the functions just defined
}
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()                     ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
