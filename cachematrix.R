### Caching the Inverse of a Matrix
### The following functions are used to create an object that stores a matrix and caches its inverse.


### Function 1: creating a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


### Function 2: Will computes the inverse of the "matrix" created above
### If the matrix already exists and is unedited then the inverse of cache shouls be retrieved.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

### Post the initial commands -- running a test
x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()

### first run for cache
cacheSolve(m)

### second run for cache
cacheSolve(m)