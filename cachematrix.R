## Solution to R Programing Assignment
## Function to Cache the inverse of given Matrix

makeCacheMatrix <- function( x = matrix() ) 
  {  a <- NULL
  
  ## Function to store the original Matrix
  store <- function( matrix ) {
    x <<- matrix
    a <<- NULL
  }
  
  ## Function to return the original Matrix
  fetch <- function() { x }
  
  ## Function to Store the inverse Matrix
  storeinv <- function(inv) { a <<- inv  }
  
  ## Function to return the inverse Matrix
  fetchInv <- function() {  a  }
  
  ## Return a list of the functions defined above
  list(store = store, fetch = fetch,storeinv = storeinv,fetchInv = fetchInv)
}

cacheSolve <- function(x, ...) {
  
  ## Return the inverse matrix of x
  m <- x$fetchInv()
  
  if( !is.null(m) ) {
    message("Loading data")
    return(m)
  }
  
  d <- x$fetch()
  
  ## inverse using matrix multiplication
  m <- solve(d, ...)
  
  ## Store the inverse to an object
  x$storeinv(m)
  
  ## Return the matrix
  m
}
