## Function to create a special matirx
## returns a list of functions as follows:
## 1. Set the matrix using $set()
## 2. Get the matrix using $get()
## 3. Set the inverse of the matrix using the $setinverse
## 4. Get the inverse of the matrix using $getinverse()

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


## cacheSolve returns the inverse of the special matrix
## It only evaluates the inverse if it has not been evaluated earlier, i.e., it firts check the cache for inverse

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
