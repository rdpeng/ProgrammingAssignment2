## Function  to make special matrix in cache
makemat <- function(x = matrix()) {
  i <- NULL
  set <- function(z) {
    x <<- z
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Function that returns matrix from cache

cacheinv <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

##running a test
A<- matrix(rnorm(100), 10, 10)
B<- makemat(A)
solve(A)
cacheinv(B)
