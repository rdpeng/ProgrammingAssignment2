## set and get speciel matrix which holds inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(i) {
    x <<- y
    mat <- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {
    mat <<- inverse
  }
  getinv <- function() {mat}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cheak if inverse of matrix already calculate and if its return it and if itsn't calculate it and return

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getinv()
  if (!is.null(mat)) {
    message('getting cached data')
    return(mat)
  }
  data <- x$get()
  mat <- sovle(mat)%*%mat
  x$setinv(mat)
  mat
}
