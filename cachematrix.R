## Put comments here that give an overall description of what your
## functions do

## Function which calculates the inverse of a matrix and caches the matrix.

makeCacheMatrix <- function(x = matrix()) {

  ## Constructor: set inv to NULL
  inv <- NULL

  ## Setter function.  Overwrite any current values with new matrix value.  Set inv to NULL in function workspace.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  ## Getter function, return the matrix.
  get <- function() x

  ## Sets matrix inverse.  Variable inv is set to the calculated matrix inverse.
  setSolve <- function(solve) inv <<- solve

  ## Gets matrix inverse.  Returns value stored in variable inv.
  getSolve <- function() inv

  ## List of functions available to a makeCacheMatrix object
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Function which retrieves the inverse of matrix x. Matix x is an object of 
## type makeCacheMatrix.  First it checks the cached value.  If no cached value 
## is found, the matrix inverse is computed and cached.

cacheSolve <- function(x) {

  ## Uses makeCacheMatrix getSolve function to retrieve the value of inv for matrix x
  inv <- x$getSolve()

  ## Test value of inv.  If inv is not NULL, matrix inverse has been cached, cached value of inv is returned.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  ## No cached value for matrix x found.  Uses getter function to retrieve the matrix.
  data <- x$get()

  ## Call solve function to calculate the inverse of a matrix.
  inv <- solve(data)

  ## Cache the matrix inverse
  x$setSolve(inv)


  inv
}
