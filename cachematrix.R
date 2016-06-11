## The following script has two functions defined for calculating the
## inverse of a square matrix and the inverse is cached for repeated use

## Create a matrix object. It is essentially creating a list of getters
## and setters for the matrix and its inverse.

makeCacheMatrix <- function(x = matrix())
{
  inverse <- NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## Calculating the inverse of the matrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  a <- x$getInverse()
  if(!is.null(a))
  {
    message("Returning cached inverse")
    return(a)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}
