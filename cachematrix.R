#The makeCacheMatrix function creates a special "matrix" object that can cache 
#its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInver <- function(Inver) m <<- Inver
  getInver <- function() m
  list(set = set, get = get,
       setInver = setInver,
       getInver = getInver)
}

#The cacheSolve function computes the inverse of the special "matrix" returned by
#makeCacheMatrix.If the inverse has been previously calculated (the matrix has
#not changed), then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix)
  x$setInver(m)
  m
}
