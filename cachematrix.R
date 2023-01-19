## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The following function creates a matrix which is a list of function
## to set and get matrix and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    in_verse <- NULL
    set <- function(y) { # setting the matrix to the object
      x <<- y
      in_verse  <<- NULL
    }
    get <- function() x # getting the object value
    # setting the mean of the matrix
    setinverse <- function(inverse) in_verse <<- inverse 
    # getting the mean of the matrix
    getinverse <- function() in_verse 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }



## Write a short comment describing this function
## The following function calculates the inverse of a matrix
## if not present in the cache else returns value from cache

cacheSolve <- function(x, ...) {
 
    in_verse <- x$getinverse() # getting the value of matrix inverse
    if(!is.null(in_verse)) { # if the inverse is calculated already
      message("getting cached data")
      return(in_verse) # retuning the value
    }
    matrix_to_invert <- x$get() # getting the matrix
    in_verse <- solve(matrix_to_invert, ...) # solving inverse of a matrix
    x$setinverse(in_verse) # setting the value
    in_verse # return the value to console
  }
        ## Return a matrix that is the inverse of 'x'

