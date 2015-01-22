## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a matrix cache to store the matix inverse and four function to
## sets the value of the matrix
## gets the value of the matrix
## sets the valuve of the matrix inverse
## gets the value of the matrix inverse if calculated already

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
  set <- function(y){ ## sets the value of the matrix
    x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed
    m <<- NULL ## sets the value of m (the matrix inverse if used cacheSolve) to NULL
  }
  get <- function() x ## gets the value of the matrix
  setmatrix <- function(solve) m<<- solve ## sets the value of the inverse in the cache
  getmatrix <- function() m ## gets the value of the matrix inverse if calculated already
  list (set = set, get = get, ## creates a list to house the four functions
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## This function calculates inverse of the matrix created in the above function. 
## First it checks if the inverse has already been calculated, if it has, the inverse is retrieved from the cache. 
## otherwise it calculates the inverse of the matrix and stores it in the cache via the setmatrix function

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()## checks to see if matrix inverse already calculated
  if(!is.null(m)){ 
    message("getting cached data")## if inverse already in cache, prints message "getting cached data"
    return(m)  ## then returns cache inverse
  }
  matrix <- x$get () ##if inverse not in cache
  m <-solve(matrix, ...) ## calculates inverse
  x$setmatrix(m) ##sets inverse in cache
  m ##returns calculated inverese on console
}
