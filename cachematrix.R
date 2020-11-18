##The makeCacheMatrix function stores a matrix in a list and defines the functions to change its value and 
##get it (as getters and setters in OOP), it also stores the value of the inverse (inizialited to NULL)
##and functions to set and retrieve its value.

##makeCacheMatrix:
##set function: This function  It makes another object y in the same enviroment
#as the input object x i.e if the value of y changes so x.
##get function: Function that returns the value of x.
##setSolve function: create an instance to the function solve i.e every time is called it computes
##the mean and sets the local variable to the inverse of the matrix.
##getSolve function: function to return the value of m (the inverse matrix).
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


##The input of this function is a variable of class list returned
##by makeCacheMatrix. It tries to retrieved the stored inverse by using
##the getSolve method in the input.If the inverse has not been computed yet (NULL), it is 
##computed and the value is stored in the input  via the setSolve 
##function. Finally the inverse matrix is returned.   

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

##R version 3.6.2
