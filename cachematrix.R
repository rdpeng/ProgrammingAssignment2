## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. This pair of functions cache 
## the inverse of a matrix.

## makeCacheMatrix creates a special "vector" which is really a list containing functions to:
## 1) set the value of an input matrix
## 2) get the value of an input matrix
## 3) set the value of the inverse
## 4) get the value of the inverse
## An assumption made for this function is that the matrix is always invertible
## This code has essentially be translated from the mean example given in the assignment
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse
  inver <- NULL
  
  ## Store the value of the input matrix into cache
  set <- function(y) {
      x <<- y
      inver <<- NULL
  }
  
  ## Get the value of the input matrix from cache
  get <- function() x
  
  ## Store the inverse of the matrix into cache
  setinverse <- function(solve) inver <<- solve
  
  ## Get the inverse of the matrix from cache
  getinverse <- function() inver
  
  ## Store all of those functions list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates the inverse of 'x' using the list created in the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse matrix in the cache via the setinverse
## function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Check to see if the inverse is already in the cache
    inver <- x$getinverse()
    
    ## If the inverse is in the cache, put a message which states we got the inverse from
    ## the cache, and return the inverse matrix
    if(!is.null(inver)) {
        ## Notify the user the inverse was already calculated and is just being taken from cache
        print("Inverse was already calculated and stored in cache. Getting cached inverse.")
        return(inver)
    }
    ## Otherwise get the inverse of the matrix, and notify the user the inverse is being calculated
    ## in this run.
    else {
        ## Notify the user that this is the first run and that the inverse is being calculated
        print("First time inverse is being calculated")
        ## Get the value of the matrix
        data <- x$get()
        ## Solve for the inverse of the matrix
        inver <- solve(data)
        ## Now set the inverse of that matrix in the cache for future use
        x$setinverse(inver)
        ## Return that inversed matrix
        inver
    }
}
