# This is a function for coursera Programming in R on-line
# class with code rprog-016 Assignment 2.
# Mz goal is to create a Caching the Inverse of a Matrix.
# I used the example version and changed in a proper way:

makeCacheMatrix <- function(x = matrix()) {
  
  storeinv <- NULL # The goal of storeinv is to store results of an inverted function
  
  set <- function(y) {
    x <<- y
    storeinv <<- NULL # Similarly to the comment above.
  }
  
  get <- function() x # It gives us the return matrix.
  settinginv <- function(inv) storeinv <<- inv # It initiates inverse matrix.
  gettinginv <- function() storeinv # It gives us the inverse matrix.
  list(set = set, get = get,
       settinginv = settinginv,
       gettinginv = gettinginv)
}


cacheSolve <- function(x, ...) {
  m <- x$gettinginv() # It gives us the inverse matrix.
  # If the calculation get wrong, it will stay NULL (see above).
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) # It calculates the inversion.
  }
  data <- x$get() #It assigns given data.
  m <- solve(data) 
  x$settinginv(m) 
  m # m returs the results.
}
