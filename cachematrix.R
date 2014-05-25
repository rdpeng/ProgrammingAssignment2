makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(cMatrix) m <<- cMatrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

##TEST CODE WORKS

##amatrix = makeCacheMatrix(matrix(c(10,2,3,4,5,6,7,8,9), nrow=3, ncol=3))
##amatrix$get()         # Returns original matrix
##cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
##amatrix$getinverse()  # Returns matrix inverse
##cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse

