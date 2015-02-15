## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix creates special vector with 4 functions
## get & set functions to get and set the matrix
## getInv  to provide inverse of a matrix from cache if is already computed
## otherwise comptute and provide
## setInv set the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
         x <<- y
         m <<- NULL
        }
        get <- function() x
     setInv <- function(inv) m <<- inv
      getInv <- function() m
 										        list(set = set, get = get,
										             setInv = setInv,
										             getInv = getInv)
  }

## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
 
 #Retrun inverse of matrix 'x'
 m <- x$getInv()
 if (!is.null(m)) {
     message("Getting cached data ....")
     return(m)
  }
  data <-x$get()
  m <-solve(data)
  x$setInv(m)
  m
  }
