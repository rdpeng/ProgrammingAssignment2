  ##THE GOAL OF THIS ASSIGNMENT IS TO WRITE A PAIR OF FUNCTIONS THAT CACHE THE INVERSE OF A MATRIX.


 ##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
     x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


  ## Function cacheSolve : Return a matrix that is the inverse of 'x' ,If the inverse has already been calculated 
                   ##then the cachesolve should retrieve the inverse from the cache.
  
 cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if(!is.null(inv)) {
      message("getting cached matrix")
      return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  }
  
  ## Program Check
MyMatrix <- matrix(rnorm(36),7,7)
M <- makeCacheMatrix(MyMatrix)
MyInvertedMatrix <-cacheSolve(M)

  ## Results
MyMatrix
MyInvertedMatrix
  
