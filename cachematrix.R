## caching the inverse of matrix
## the user needs to first cache the matrix using makeCacheMatrix
## and then pass the returned value to cacheSolve to find the inverse of matrix


## this function returns the function in the form of list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m<<- solve
  getmatrix <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## once the matrix is cached the inverse need not be calculated again and again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
          message('getting cached data')
          return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setmatrix(m)
        m
}
