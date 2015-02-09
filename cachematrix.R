# Function to calculate the invert of a matrix (x)
makeCacheMatrix <- function(x = matrix()){
  # Variable to keep track of the cached inverterd matrix
  cached <- NULL
  # New function to change the matrix to be used (y)
  set <- function(y = matrix()){
    x <<- y
    cached <<- NULL
  }
  # Variable that contains the matrix, either x (original) or y (modified)
  get <- function() x
  # Calculates the inverted matrix from x
  setInvert <- function(solve) cached <<- solve
  # Stores the inverted matrix
  getInvert <- function() cached
  # Lists the different functions within the makeCacheMatrix function
  list(set = set, get = get, setInvert = setInvert, getInvert = getInvert)
}

# Function to return the cached or recalculated value for the invert of a matrix
cacheSolve <- function(x, ...){
  # Reading the cached invert matrix from the makeCacheMatrix function
  cached <- x$getInvert()
  # Evaluates whether the value of the variable cached is not NULL 
  if(!is.null(cached)){
    message("Recovering the chached data")
    return(cached)
  }
  # If the value from cahed is NULL the new matrix is read into a new variable (newMatrix)
  newMatrix <- x$get()
  # The value of the variable cached, which keeps track of the inverted matrix, is rewrited 
  # to the value of the inverted matrix
  cached <- solve(newMatrix)
  # Calculates the inverted matrix from x
  x$setInvert(cached)
  # Print the inverted matrix
  return(cached)
}
