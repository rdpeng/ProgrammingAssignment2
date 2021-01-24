## A special matrix "object" is created by this function that has an inverse cache.
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setanswer <- function(answer) a <<- answer
  getanswer <- function() a
  list(set = set, get = get,
       setanswer = setanswer,
       getanswer = getanswer)
}
## The special matrix's inverse is comuputed by this function.  
cacheanswer <- function(x, ...) {
  a <- x$getanswer()
  if(!is.null(a)) {
    message("solving for the inversed matrix")
    return(a)
  }
  data <- x$get()
  a <- answer(data, ...)
  x$setanswer(a)
  a
}
