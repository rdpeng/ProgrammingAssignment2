## Put comments here that give an overall description of what your
## functions do

## In fact functions nead only to change computation function from mean to solve, rest works at the same way, with setters and getters. 
## Small difference is also an function argument, that is not a vector, but matrix, as per assignement, there is not need to check whenever
## matris is invertable, as we assume it is. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(pupa) m <<- pupa
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## This is function, that calculates inverse (solve) if not yet inverted erlier. 

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
