## The first function creates an inverted matrix, and initiates some data labels. The second one checks if the inverted matrix exists, and inverts it if not. 

## The function creates an inverted matrix, and initiates some data labels we will use in the second function.

makeCacheMatrix <- function(x = matrix()) {
  m <<- NULL
  firstmatrix <<- x
  matrix(nrow = dim(x)[2], ncol = dim(x)[1])
}


## The function checks if the inverted matrix exists, return a message and the inverted matrix if do, and inverts the matrix if not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m <<- matrix(vapply(firstmatrix, c, FUN.VALUE = vector(mode = "numeric", length = 1)),nrow = dim(firstmatrix)[2], ncol = dim(firstmatrix)[1], byrow = TRUE)
  m
}
