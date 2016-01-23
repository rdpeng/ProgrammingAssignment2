## Sets the value of a matrix and when the 
## inverse is computed, saves the inverse in setinverse
## x: the value of the matrix to save
## set: sets the matrix
##get: gets the matrix 
##setinverse: saves the inverse matrix
##getinverse: gets the inverse matrix, null if it does not exist
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a makeCacheMatrix object 
## computes the inverse of the matrix ands saves it
## in the cacheMatrix object
## x: the makeCacheMatrix object

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached inverse matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
