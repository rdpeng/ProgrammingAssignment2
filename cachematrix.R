## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y){
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinversa <- function(inver) inversa <<- inver
  getinversa <- function(mean) inversa
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)

}


## Write a short comment describing this function

cacheinversa <- function(x, ...) {
  inversa <- x$getinversa()
  if(!is.null(inversa)){
    message("obtener desde cache")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinversa(inversa)
  inversa
        ## Return a matrix that is the inverse of 'x'
}
