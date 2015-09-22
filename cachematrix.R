##makeCacheMatrix function takes a numeric square matrix as input and returns the four funcitons
##set, get, setinv and getinv as a list to the variable. the variable can be passed to cacheSolve function 
##as argument to the either the cached matrix inverse or newly calculated matrix inverse


makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(mat_inv) i <<- mat_inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function requests cached inverse matrix value through getinv function. 
## If the value is not found,then it calculates the matrix inverse through built in 
## R function 'solve'. cacheSolve function accepts a list containing the four functions
## as column value as the arguments

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

