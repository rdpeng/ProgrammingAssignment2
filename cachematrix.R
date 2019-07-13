## The first function, makeCacheMatrix, creates a list which includes the
## functions to generate;
## a cache of the inverse of a matrix , the inverse if it exists, the function
## name that generates the inverse and the actual matrix. 



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


## This function,  checks to see if the function above has generated an inverse
## if so it returns that solution, otherwise it uses the function 'solve' to 
## find the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

