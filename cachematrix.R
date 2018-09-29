## makeCacheMatrix and cacheSolve functions

## makeCacheMatrix: This function creates and stores in the parent environment both the inputed matrix and its inversion

makeCacheMatrix <- function(x = matrix()) { #Defines the makeCacheMatrix function with the variable x as its argument
  inv <- NULL # Creates the "inv" object to be used in a later line
  set <- function(y) { # Creates the "set()" function
    x <<- y # Assigns the inputed matrix argument to the variable "x" in the parent environment
    inv <<- NULL # Clears the cached value of "inv"
  }
  get <- function() x # The value of "x" is retrieved from the parent environment. Getter for "x".
  setsolve <- function(solve) inv <<- solve # Setter for "inv". Assigns the value "inv" of the modified matrix in the parent environment
  getsolve <- function() inv # Getter for "inv". 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) # Creates the list with Getters and Setters in the parent environment
}

## cacheSolve: This function retrieves (if available) or calculates and stores the inversion of the inputed matrix

cacheSolve <- function(x, ...) { # Creates the function "cacheSolve" which retrieves or creates the inversion of the inputed square matrix
  inv <- x$getsolve() # Tries to retrieve the inverted matrix
  if(!is.null(inv)) { # Checks if the inverted matrix exists in the parent environment
    message("getting cached data") # If the inverted matrix exists, prints a code confirming it
    return(inv) # Returns the cached data in the parent environment
  }
  data <- x$get() # If the inverted matrix does not exist in the parent environment, retrieves the inputed matrix
  inv <- solve(data, ...) # Inverts the inputed matrix
  x$setsolve(inv) # Assigns the inverted matrix to "inv"
  inv # Returns "inv"
  ## Return a matrix that is the inverse of 'x'
}
