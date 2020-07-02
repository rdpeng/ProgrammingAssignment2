## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  spmx <- NULL
  set <- function(y){
    x <<- y
    spmx <<- NULL
  }
  get <- function() x
  setim <- function(solve) spmx <<- solve
  getim <- function() spmx
  list(set=set, get=get, setim=setim, getim=getim)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  spmx <- x$getim()
  if(!is.null(spmx)){
    message("getting cached data")
    return(spmx)
  }
  data <- x$get()
  spmx <- data**-1
  x$setim(spmx)
  spmx
}
