## cache means to essentially store data so that requests for data can occur at a faster rate.

## This function allows you to set and retrive the value of the vector and means

makeCacheMatrix <- function(x = matrix()) {
  mc <- NULL
  set <- function(y) {
    x <<- y
    mc <<- NULL
  }
  
  get <- function() x
  setmean <- function(mean) mc <<-mean
  getmean <- function() mc
  list( set = set, get = get,
        setmean = setmean,
        getmean = getmean) ## call functions individually
}


## This function actually caculates the mean but looks at the previous function to see whether the mean has already been cacualted. 

cacheSolve <- function(x=matrix(), ...) {
  mc <- x$getmean()
  if(!is.null(mc)) {
    message("This is now getting cached data")
    return(mc)
  }
  info <- x$get()
  mc <- mean(matrix, ...)
  x$setmean(mc)
  mc
}
