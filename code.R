#the first function is used to define the variables and procedures required to inverse
#every matrix. 'x' will be the matrix to be inverted and 'm' is the object used inside to make this invertion

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#The second function is the procedure itself. In this case, 'm' will be the inverse matrix 

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}