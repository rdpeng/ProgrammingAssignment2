## This function is getting a matrix and creating getters and setters to a matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL #This is setting the solve to NULL, creating a NULL variable.
  set <- function(y) {
    x <<- y #Changing the value of the Matrix
    message("resetting inverse to NULL") 
    s <<- NULL #Resetting the Inverse to NULL
  }
  get <- function() x #Getting the value of the matrix
  setsolve <- function(solve) s <<- solve #Changing the value of the inverse
  getsolve <- function() s #Getting the value of the inverse
  #This is returning the setters and getters
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
#This function calculates the inverse of the matrix and caches it using the setsolve
cacheSolve <- function(x, ...) { 
  s <- x$getsolve() #This is getting the inverse
  if(!is.null(s)) { #This is testing if the inverse is calculated already
    #If it was (not NULL), it will load it and return it
    message("getting cached inverse")
    return(s)
  } 
  #Otherwise, it will load the data...
  data <- x$get()
  s <- solve(data, ...) #... Calculate the inverse and 
  x$setsolve(s) #Set the new inverse
  s
}
