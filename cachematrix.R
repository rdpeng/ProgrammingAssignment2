## Caching the Inverse of a Matrix:

## second programming assignment will require 
## you to write an R function is able to cache 
## potentially time-consuming computations. 
## For example, taking the mean of a numeric 
## vector is typically a fast operation. However, 
## for a very long vector, it may take too long 
## to compute the mean, especially if it has to be 
## computed repeatedly (e.g. in a loop). If the 
## contents of a vector are not changing, it may 
## make sense to cache the value of the mean so 
## that when we need it again, it can be looked up 
## in the cache rather than recomputed. In this 
## Programming Assignment will take advantage of 
## the scoping rules of the R language and how 
## they can be manipulated to preserve state inside 
## of an R object.

## setting the value of the matrix
## getting the value of the matrix
## setting the value of the inverse
## getting the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
      x <<- y
      n <<- NULL
    
  }
  get <- function() x
  setinv <- function(inverse) n <<- inverse
  getinv <- function() n
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the 
## "matrix" created by makeCacheMatrix above. 
## If the inverse was already calculated, then it 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  n <- x$getinv()
  if(!is.null(n)) {
      message("getting cached inverse matrix")
      return(n)
      } else{
          data <- x$get()
          n <- solve(data, ...)
          x$setinv(n)
          return(n)
      }
}
