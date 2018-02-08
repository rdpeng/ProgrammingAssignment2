## makeCacheMatrix builds a set of functions and returns the functions within
## a list to the parent environment.  If you run the code, you will notice that
## each element in the list has the same parent environment (i believe they are 
## all in the makeCacheMarix environment).  This is because of the <<- operator in
## the set function.  
makeCacheMatrix <- function(x = matrix()) {
  
  if(!is.matrix(x)) {stop("x must be a matrix")} # Test to see whether x is a matrix (optional)
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve is a function that is used to complete the makeCacheMatrix function.  It is used to
## retrieve the object (inverse) in the makeCacheMatrix object.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

p <- matrix(1:4, 2, 2)  # Creating a square matrix
q <- makeCacheMatrix(p) # Creating a variable that will be used in the cacheSolve function
q  # As mentioned above, each element in the list has the same environment

cacheSolve(q) # Inverse of matrix

cacheSolve(q)  # The cached data matrix