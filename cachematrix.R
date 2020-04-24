## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##The function makeVector creates a special "matrix"
makeCacheMatrix <- function(x = matrix()) {

  
    m <- NULL
    ##set the value of the matrix

    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ##get the value of the matrix

    get <- function() x
    
    ##set the value of the inverted matrix
    setmat <- function(solve) m <<- solve
    
    ##get the inverted matrix
    getmat <- function() m
    list(set = set, get = get,
         setmat = setmat,
         getmat = getmat)
  
}


## Write a short comment describing this function
##The following function calculates the inverse  of the special "matrix" created with the above function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getmat()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmat(m)
    m
  }
  

