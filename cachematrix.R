## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## This function caches the inverse of a matrix
 makeCacheMatrix <- function(x = matrix ()) {
      m <- NULL
      set <- function (y) { #setting the value of the input matrix
           x <<- y
           m <<- NULL
      }
      get <- function () x  #getting the value of the matrix
      setSolve <- function(solve) m <<- solve  # setting the value of the inverse of the matrix
      getSolve <- function (m)  # getting the value of the inverse matrix
      list (set = set, get = get,  # saving the cached matrix and it inverse in the special matrix type
           setSolve = setSolve,
           getSolve = getSolve
           }

## Write a short comment describing this function
## function to calculate the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()  # inserts into m the value of getSolve from the special matrix cache
    if (!is.null(m)) { # checks if the matrix exists in the special matrix prepared in makeCacheMatrix
        message("getting cached data")
        return (m)
     }
     data <- x$get()  # getting the inverse from the cache
     m <- solve(data, ...)  # setting the inverse of the matrix
     x$setSolve(m)  #  setting the inverse of the matrix from the cache
     m       
}
