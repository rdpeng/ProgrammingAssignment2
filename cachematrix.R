## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {       ## define the argument with default mode of "matrix"
  m <- NULL                                       ## initialize inv as NULL; will hold value of matrix inverse 
      set <- function(y) {                        ## define the set function to assign new
            x <<- y                               ## value of matrix in parent environment
            m <<- NULL                            ## if there is a new matrix, reset inv to NULL
      }
      get <- function() x                         ## define the get fucntion - returns value of the matrix argument
      setinverse <- function(solve) m <<- solve   ## assigns value of inv in parent environment
      getinverse <- function() m                  ## gets the value of inv where called 
      list(set = set, get = get,                  ## you need this in order to refer to the functions with the $ operator
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {                 
   ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()                                   ## get inverse
      if(!is.null(m)) {                                 ## if inverse exists, check if already cached 
          message("getting cached data") 
          return(m)                                     ## if yes, return cached inverse
      }
      data <- x$get()                                   ## if not, get matrix
      m <- solve(data, ...)                             ## compute inverse of matrix
      x$setinverse(m)                                   ## cache inverse of matrix
      m     
}
