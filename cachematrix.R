## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list contains matrix,
# set the matrix, set inverse matrix and get inverse matrix
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
# This function returns inverse of matrix,
# First it checks inverse already exists, and if it already computed,
# function returns the value. if not it computes inverse matrix using solve function
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