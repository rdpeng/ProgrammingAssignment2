### The Functions do what requestet by ProgrammingAssisment2


## This function create a Matrix and a Vector of four functions
#set the values and dimension of the matrix
#get the values and dimension of the matrix
#set the values and dimension of the inverse matrix
#get the values and dimension of the inverse matrix

makeCacheMatrix <- function(x = numeric(), y = numeric(), z = numeric()) {
  matrix <- matrix(x,nrow = y ,ncol = z)
  s <- NULL
  set <- function(x = numeric(), y = numeric(), z = numeric()) {
    matrix <<- matrix(x,nrow = y ,ncol = z)
    s <<- NULL
  }
  
  get <- function()
    matrix
  setsolve <- function(solve)
    s <<- solve
  getsolve <- function()
    s
  list(
    set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}


cacheSolve <- function(x) {
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}


## returns and stores the inverse matrix of the matrix created by makeCacheMatrix()

cacheSolve <- function(x) {
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}


