## [The following functions creates a special matrix along with few functions which enable caching of time consuming matrix inverse
##   operation.
##  To leverage the functions one has to first assign or create the matrix using makeCacheMatrix.
##   e.g. b <- makeCacheMatrix( matrix(c(....), nrow = .. , ncol = ..))]
##  To compute the inverse using cached process leverage function cacheSolve
##   e.g. cacheSolve(b) - the solution would be a matrix inverse based on fresh computation or by retrieving the matrix 
##   from the cache. The matrix retrieved from cache will begin with "getting cached data"


## makeCacheMatrix - creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # set - assign the value of matrix
  # get - get value of matrix
  # setInverse - set value of matrix or compute the inverse
  # getInverse - get value of matrix which is cached primarily the inverse
  
    m <- NULL
    set <- function(y) {
      x <<- y               # assign the new matrix to x
      m <<- NULL            # reset the matrix value
    }
    get <- function() x     #return matrix which is saved
    setInverse <- function(solve) m <<- solve   #using solve since it will compute matrix inverse
    getInverse <- function() m    # returns / retrieves inverse matrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)    #return the special matrix with defined functions
  }


## cacheSolve - This function will either return the cached matrix inverse or computes matrix inverse + stores it in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()   
  if(!is.null(m)) {
    message("getting cached data")
    return(m)             # returns the cached value if it exists
  }
  
  
  data <- x$get()    # in case the cache doesn't exist then Step 1 - we retrieve the matrix
  m <- solve(data)   # Step 2 - compute the matrix inverse using solve R function
  x$setInverse(m)    # Step 3 - Store the matrix inverse in cache
  
  m                  # Return the matrix inverse
}
