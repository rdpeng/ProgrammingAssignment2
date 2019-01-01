makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
#testing above functions below 

#matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#matrix$get()
#matrix$getInverse()
#cacheSolve(matrix)
#matrix$getInverse()
#matrix$set(matrix(c(2, 1, 1, 4), 2, 2))
#matrix$getInverse()
#cacheSolve(matrix)
#matrix$ getInverse () 


