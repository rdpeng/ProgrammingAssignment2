## My two functions, makeCacheMatrix and cacheSolve first create a special matrix 
## that has a variable to store the inverse the matrix once it has been calculated.
## Then cacheSolve checks first if that inverse is already saved and otherwise calculates
## and then saves the inverse of the original matrix. The cacheSolve function then returns
## the inverse of the original matrix.

## First the makeCacheMatrix creates an empty variable "inv". Then it defines the function "set"
## to create the variable "inv" outside of the original environment, then it defines the function 
## "get" to get the data. Then it defines the "setinv" function to save
## the calculated inverse of the marix given and defines the "getinv" function to retrieve the calculated inverse. 
## Lastly, it creates a list of these functions that is added as a variable to the original matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## The function "cacheSolve" returns a matrix that is the inverse of "x". First 
## it retrieves the saved inverse of "x" using the function "getinv" and passes it to "inv".
## Then, if that saved inverse is not NULL it returns a message and the saved inverse of "x'. 
## If "inv" is NULL, which means that inverse has not yet been saved, then
## it calls the data using the "get" function and solves for the inverse of that data.
## The function saves that calculated inverse and then returns in the inverse.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("I am calling solve")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
