## The following program has two functions: 
## makeCacheMatrix: Takes a matrix as input and stores its inverse in cache
## cacheSolve: Takes a matrix as input and checks to see if it's inverse is
##            available in cache otherwise computes the inverse and stores it in
##            cache (using first function). Returns the inverse of the matrix


## Function makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setinv <- function(inv) mat <<- inv
  getinv <- function() mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
