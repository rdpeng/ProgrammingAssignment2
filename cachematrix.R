## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# I just changed little things using makeVector() and cachemean()function
# The only thing I needed to change was the name of functions and variables
# so, I put solve() function to get caculation of inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y) {
                x <<- y
                 m <<- NULL
         }
         get <- function() x
         setc <- function(solve) m <<- solve
         getc <- function() m
         list(set = set, get = get,
              setc = setc,
              getc = getc)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getc()
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
         m <- solve(data, ...)
         x$setc(m)
         m
}