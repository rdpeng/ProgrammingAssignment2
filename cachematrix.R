## Function to create a cacheable matrix along with its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize a variable to store the inverse
  inv <- matrix()
  
  # Function to set the matrix and its inverse
  set <- function(y) {
    # Assign the matrix to 'x' in the enclosing environment
    x <<- y
    # Initialize the inverse matrix
    inv <<- matrix()
  }
  
  # Function to retrieve the matrix
  get <- function() x
  
  # Function to compute and set the inverse
  setinv <- function(x) inv <<- solve(x)
  
  # Function to retrieve the inverse
  getinv <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function to compute the inverse of a matrix, caching the result if available
cacheSolve <- function(x, ...) {
  
  # Retrieve the cached inverse and original matrix
  inv <- x$getinv()
  
  # If the inverse is cached, print message and return the cached inverse
  if(!all(is.na(inv))) {
    message("getting cached data")
    return(inv)
  }
  
  # Retrieve the matrix
  data <- x$get()
  
  # Compute the inverse
  inv <- x$setinv(data)
  
  # Return the computed inverse
  inv
}

# Tests
# Setting inverse on cache before running cache solve
m <- matrix(c(2, 1, 3, 4), 2, 2)
inverse1 <- makeCacheMatrix()
inverse1$set(m)
original1 <- inverse1$get()
inverse1$setinv(original1)
inverse1$getinv()
cacheSolve(inverse1)

# Solving for inverse only on cache solve
inverse2 <- makeCacheMatrix()
inverse2$set(m)
original2 <- inverse2$get()
cacheSolve(inverse2)


