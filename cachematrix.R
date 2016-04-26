## A pair of functions that cache the inverse of a matrix
## makeCacheMatrix:creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  m<-NULL
  
  ## Method to set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    m
  }
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m      
}
