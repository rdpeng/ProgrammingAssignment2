## makeCacheMatrix function creates an object that stores a square matrix and 
## another object that stores its inverse
## cacheSolve function either retrives the cached inverse from makeCacheMatrix or recalculates the inverse
## for the new argument of makeCacheMatrix

## The function makeCacheMatrix contains two data objects x and I and produces four functions
## set(), get(), setinverse(), getinverse()

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calls getinverse (from the function above) and checks if it already 
## is cached and populates it or otherwise recalculates the inverse of get function and populates it

cacheSolve <- function(x, ...) {
  I <- x$getinverse()
  if (!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
