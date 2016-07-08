# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    invs<-NULL
    set<-function(y){
      x <<- y
      invs <<- NULL
    }
    get <-function() x
    setinverse <-function(inverse) invs <<- inverse
    getinverse <-function() invs
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

# The following function returns the inverse of the matrix.First it checks if
# the inverse has already been computed. If yes, it gets the result and skips the
# computation. If no, it computes the inverse, sets the value in the cache via
# the function setinverse.

# This function assumes that the matrix is always invertible(and square).

cacheSolve <- function(x, ...) {

  invs <- x$getinverse()
  # if the inverse has already been calculated
  if(!is.null(invs)) {
    # get it from the cache and skips the computation.
    message("getting cached data")
    return(invs)
  }
  # otherwise, calculates the inverse 
  data <- x$get()
  invs <- solve(data, ...)
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(invs)
  invs
        ## Return a matrix that is the inverse of 'x'
  
}

## Test function
## r = rnorm(1000000)
## mat1 = matrix(r, nrow=1000, ncol=1000)
## invs= makeCacheMatrix(mat1)
## invs$get()
## cacheSolve(invs)


