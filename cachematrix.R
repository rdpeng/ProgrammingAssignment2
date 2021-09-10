## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function(mean) m
  list(set = set, get = get,
       setmean = seatmean,
       getmean = getmean)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)){
    message("obtener desde cache")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
