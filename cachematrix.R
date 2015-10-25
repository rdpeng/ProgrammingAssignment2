## well my functions are going to do something I really don't 
## understand yet.  Hmm well I can run square matrices through 
## these functions....  And it does similar things as the CacheVector
## example we were given.
## I used 
##       > neo <- stats::rnorm(16)
##       > dim(neo) <- c(4,4)
##       > neo
## and
##       > trinity <- stats::rnorm(9)
##       > dim(trinity) <- c(3,3)
##       > trinity
## to test

## a blue Caterpillar smoking a hookah will come and 
## magically make a chached Matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) 
}


## Neo will take the Blue pill

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   s <- x$getsolve()
   if(!is.null(s)) {
      message("Getting Cached Matrix Inversion")
      return(s)
   }
   data <- x$get()
   s <- solve(data, ...)
   x$setsolve(s)
   s
}
