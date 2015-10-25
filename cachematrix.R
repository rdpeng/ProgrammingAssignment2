## cacheMatrix.R
## well my functions are going to do something I really don't 
## understand yet.  Hmm well I can run square matrices through 
## these functions....  And it does similar things as the CacheVector
## example we were given.
## I used 
##       > neo <- stats::rnorm(16)
##       > dim(neo) <- c(4,4)
##       > neo
##       > a <- makeCacheMatrix(neo)
## ,
##       > trinity <- stats::rnorm(9)
##       > dim(trinity) <- c(3,3)
##       > trinity
##       > b <- makeCacheMatrix(trinity)
## and
##       > alice <- c(1,2,4,8,1.2,2.4,4.8,8.1,16)
##       > dim(alice) <- c(3,3)
##       > alice
##       > c <- makeCacheMatrix(alice)
## to test

## makeCacheMatrix <- function(x = matrix())
## a blue Caterpillar smoking a hookah will come and 
## magically make a chached Matrix.  A list of functions
## will be produced.  The '<<-' superassignment operator
## is used to assign variables outside of the local function
## environment

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

## cacheSolve <- function(x, ...)
## Neo will take the Blue pill
##
## This will return cached solve values if they are found
## Otherwise it will take a new solve inversion of the matrix 
## and store it in cache

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
