## The function makes a special vector that users can cache its inverse.

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setvalue <- function(value) a <<- value
  getvalue <- function() a
  list(set = set, get = get,
       setvalue = setvalue,
       getvalue = getvalue)
}

## The function computes the inverse of a special vector by the aforementioned function. 

cacheSolve <- function(x, ...) {
  a <- x$getvalue()
  if(!is.null(a)) {
    message("inverse cache matrix")
    return(a)
  }
  data <- x$get()
  a  <- value(data, ...)
  x$setvalue(a)
  a
}
