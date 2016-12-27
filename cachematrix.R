##Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. makeCacheMatrix contains 4 functions: set, get, setmean, getmean.
##(1)get is a function that returns the vector x stored in the main function.
##(2)set is a function that changes the vector stored in the main function.
##(3)setmean and getmean are functions very similar to set and get.
##(4)They don't calculate the mean, they simply store the value of the input in a variable m.
##(5)into the main function makeVector (setmean) and return it (getmean).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { 
    ## use `<<-` to assign a value to an object in an environment 
    ## different from the current environment. 
    ## if user want to reset matrix 
    x <<- y  ## reassign "new" matrix to x 
    m <<- NULL  ## reinitialize m to NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##Function "cacheSolve" computes the inverse of the special "matrix" (which is the input of cachemean) returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  m <- x$getinverse()
  ## if the inverse has already been calculated
  if(!is.null(m)) {  ## if user had calculated the same matrix before
    message("getting cached data")
    ## get it from the cache and skips the computation. 
    message("getting cached data")
    return(m)  ## return old result(m) directly 
  }
  else{
  # otherwise, calculates the inverse 
  data <- x$get()    ## otherwise, get the uncalculated matrix
  m <- solve(data, ...)  ## calculate the inverse matrix
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(m)  ## reassign inverse matrix 
  m   ## print the inverse matrix 
  }
}

    