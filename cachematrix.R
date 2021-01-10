## The function below aims to create a special vector or matrix that can cache the inverse.

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
        p <- NULL
        set <- function(y) {
                x <<- y
                p <<- NULL
        }
        get <- function() x
        setmean <- function(fvalue) p <<- fvalue
        getmean <- function() m
        list(set = set, get = get,
             setfvalue = setfvalue,
             getfvalue = getfvalue)
}


## The function below aims to solve/compute for the inverse of the matrix.

cachefvalue <- function(x, ...) {
  p <- x$getfvalue()
  if(!is.null(p)) {
    message("solving for the inversed matrix")
    return(p)
  }
  data <- x$get()
  p <- fvalue(data, ...)
  x$setfvalue(p)
  p
}
