
## In makeCacheMatrixm, I set m as the NULL. The get function is used to retrieve matrix x from the parent environment   
## I then use the "Solve" function to obtain the inverse of the matrix and save it as "setinverse".
## The global environment now contains a list of 4 objects: set(), get(), setinverse() and getinverse()


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL 
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
} 

## In cacheSolve, I return a matrix that is the inverse of 'x'. It    
## I got stuck for a long case, as I tried to invoke a ifelse statement that
## compares the matrix with the matrix stored in the cache using:
## if((is.matrix(z) && is.matrix(data) && dim(z) == dim(data) && all(z == data))), 
## but it didn't work.
## data contains a matrix retrieved from the global environment


cacheSolve <- function(x, ...) {
  
  data <- x$get()  # retrieve matrix from the global enviroment 
  m_retrieved <- x$getinverse()
  if(!is.null(m_retrieved)) {
    message("getting cached data")
    return(m_retrieved
  }
  else { 
    m_calcuated <- solve(data, ...)
  }
  x$setinverse(m_calcuated)
  m_calcuated 
  
}

## I test the code using square matrix a and b and it works!
## a <- makeCacheMatrix(matrix( c(4, 2, 2,
##                                2, 3, 1,
##                                2, 1, 3), nrow=3, byrow=TRUE))
##
## cacheSolve(a) 
## 
## b <- makeCacheMatrix(matrix( c(0, 0, 2,
##                                2, 3, 1,
##                                2, 1, 3), nrow=3, byrow=TRUE))
##
## cacheSolve(a) 
## cacheSolve(b)
## cacheSolve(b)
