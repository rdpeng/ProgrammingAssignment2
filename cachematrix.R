## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

## x is a matrix here and s is NULL
##  "mean" is referencing to "mymean"


makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setmymean <- function(solve) s <<- solve
  getmyymean <- function() s
  list(set = set, get = get,
       setmymean = setmymean,
       getmyymean = getmyymean)
}


##  changed "mean" to "mymean"




cachemymean <- function(x, ...) {
  s <- x$getmyymean()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setmymean(s)
  s
}