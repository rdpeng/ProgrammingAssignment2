## Put comments here that give an overall description of what your functions do
###############################################################################
## No idea.  This assignment was completely mysterious and vague.  I even read 2
## posts that explained what it was supposed to be, but I stil didn't hav a clue.
## I did spend a lovely 4 hours playing with Git to where I could actually set up
## the fork and have files go up/down.
###############################################################################
## Helper functions provided

###############################################################################
## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
  
}

###############################################################################
## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(solve(m))
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    
    solve(m)
    
}
