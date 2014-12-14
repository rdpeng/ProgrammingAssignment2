## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m<- NULL
  set <- function(y){
    x<<- y
    m <<- NULL
  }
  get<- function()x
  setmatrix <- function(solve) m <- solve
  getmatrix <- function() m
  list(set = set, get= get, setmatix= setmatrix, getmatrix= getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
m <- X$getmatrix()
  if(!is.null(m))
  {
    print("getting cached matrix")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
