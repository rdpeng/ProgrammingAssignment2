## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## 01
  inv <- NULL
## 02
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
## 03
  get <- function(){
    x 
  } 
## 04
  setinv <- function(inverse) {
    inv <<- inverse 
  }
## 05  
  getinv <- function() {
    inv 
  }
## 06
  list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## 01
  inv <- x$getinv()
## 02
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
## 03
  data <- x$get()
## 04
  inv <- solve(data, ...)
## 05
  x$setinv(inv)
  inv
}
