## Caching the Inverse of Matrix

# Create a special Matrix object that can cache its inverse
makeCacheMatrix <- function(m = matrix()) {
  
  # Initialization of Inverse Property
  i <- NULL
  
  # Method to Set the value of Matrix
  set <- function(matr) {
    m <<- matr
    i <<- NULL
  }
  
  # Method to Get the Value of Matrix
  get <- function() m
  
  # Method to Set the Inverse of Matrix
  setinverse <- function(inverse) i <<- inverse
  
  # Metod to Get the Inverse of Matrix
  getinverse <- function() i
  
  # Return the list of Methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# If the Inverse of Matrix is already calculated and the Matrix has not changed, 
# Then It should retrieve from the cache
cacheSolve <- function(m, ...) {
  
  # Return the Inverse of Matrix 'm'
  i <- m$getinverse()
  
  # Return Inverse of Matrix from Cache
  if (!is.null(i)) {
    message("Cached Data")
    return(i)
  }
  
  # Get the matrix from the object
  data <- m$get()
  
  # The Inverse of Matrix
  i <- solve(data, ...)
  
  # Set the Inverse to the object
  m$setinverse(i)
  
  # Return the Matrix
  i
}

# Testing - Call the function with matrix
A <- matrix(1:4,2,2)
B <- makeCacheMatrix(A)

# Inverse Returned After Computation
cacheSolve(B)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

# Inverse Return from the Cache
cacheSolve(B)
## Cached Data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

