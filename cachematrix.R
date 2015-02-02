## 2 functions:
## makeCacheMatrix - creates cacheable matrix and a list of functions to manipulate it
## cacheSolve - gets the inverse value of a matrix, checking if that inverse value has already been calculated and thus available from cache

## makeCacheMatrix - creates cacheable matrix and a list of functions to manipulate it

makeCacheMatrix <- function(x = matrix()) { 
  s <- NULL ## clear old solve/invert values from previous matrixes
  get <- function() x ## function to get matrix value
  sets <- function(solve) s <<- solve ## function to SET matrix solve/invert value
  gets <- function() s ## function to GET matrix solve/invert value
  list(get = get, sets = sets, gets = gets) ##list to get all above functions
}


## cacheSolve - gets the inverse value of a matrix, checking if that inverse value has already been calculated and thus available from cache

cacheSolve <- function(x, ...) {
  s <- x$gets() ## gets current inverse value
  if(!is.null(s)) { ## checks if current inverse value exists
    message("getting cached data") ## if so, it tells you it's getting the cached value of inverse
    return(s) ## and just shows you what it is, ending the function
  } ## if current inverse value is NULL..
  data <- x$get() ## gets the matrix in question
  s <- solve(data) ## inverts matrix
  x$sets(s) ## caches inverse value
  s ## returns inverse value
}
