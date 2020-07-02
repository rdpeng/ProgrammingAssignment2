## The makeCacheMatrix function will take in a square matrix and create a list of functions that can be used for it: setting the value, getting the value, setting the inverse matrix, getting the inverse matrix
## The cacheSolve will take in the returned contents from the makeCacheMatrix and solve then cache the inverse matrix value

## The makeCacheMatrix takes in a square matrix


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


## The cacheSolve function takes in the what is returned from the makeCacheMatrix to determine the inverse of the square matrix (either cache or calculated)

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
