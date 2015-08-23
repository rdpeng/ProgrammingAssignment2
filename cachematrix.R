## The two functions in this program calculate the inverse of a matrix. 
## However, if the inverse has been previously calculated the program
## retrieves the inverse from a cache rather than recalculating.

## makeCacheMatrix contains four functions; to set the value of the matrix, 
## to get the value of the matrix, to set the value of the inverse function, 
## and to get the value of the inverse function

makeCacheMatrix <- function(x = matrix(x <- c(), ncol = sqrt(length(x)))) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}

