Tilte: "Caching the Inverse of a Matrix"
Author: "Marida Gingras"
Date: "September 9,2016"
Output: "Github"
R-Programming Assignment 2
makeCacheMatrix <- function(x = numeric ()){  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<-NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}

akeVector <- function(x = numeric ()){
  m<- NULL
  set <- function(y){
    x <<- y
    m <<-NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean)
  getmean <- function() m
  list(set = set, get = get,
     setmean = setmean,
     getmean = getmean)
}


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
makeCacheMatrix()

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

  
