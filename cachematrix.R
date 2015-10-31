## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function sets and gets the matrix and inverse values 
CacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmtx <- function(matrix) m <<- matrix
  getmtx <- function() m
  list(set = set, get = get,
       setmtx = setmtx,
       getmtx = getmtx) 
}


## Write a short comment describing this function
## This function checks whether the matrix exists
## If it does, it gets the calculation from cache
## otherwise it calculates the martix
## then it inverses (solves) the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmtx()
  if(!is.null(m)) {
    message("getting cached data")
    minv <- solve(m)
    return(minv)
  }
  data <- x$get()
  m <- matrix(data, ...)
  x$setmtx(m)
  minv <- solve(m)
  minv
}
