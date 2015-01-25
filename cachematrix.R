## Put comments here that give an overall description of what your
## functions do
###New comments
####Make vector to take mean of, How about cats in room

cats <- function(x = numeric() ){
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

## Write a short comment describing this function
### catch the cached cats

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



makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function
### I am out of time to catch the cats and debug this program


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
