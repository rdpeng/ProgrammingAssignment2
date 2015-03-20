## Dear Reviewer - Thank you for taking the time to help my learning process!  
## If you are wondering whether your QA is worthwhile, please check out this cartoon
## http://blogs.catapultsystems.com/BA/Lists/Photos/042511_1431_MetricMisus1.gif

## The functions optimize speed for providing an Inverse of a Matrix.
## The overall process is:
## See if an answer is in the cache
## If yes, provide that.
## If not, provide the Inverse via Solve, put it in the cache, and then return it

## This function returns a list of 4 functions:
## Set, Get, SetInv and GetINV

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function searches for matrix X in the cache.  If it's there,
## the job is done.  If not, it calculates the inverse with Solve
## puts it in the cache, and then returns the answer.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
