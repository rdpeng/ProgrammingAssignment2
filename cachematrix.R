## Put comments here that give an overall description of what your
## functions do
## This function returns a list with all 4 functions and stores the inverse in parent environment
##i <- NULL begins by setting the inverse to NULL as a placeholder for a future value
##set <- function(y) {x <<- y; m <<- NULL} defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, i, to NULL
##get <- function() x returns the matrix, x
##setinverse <- function(inverse) i <<- inverse sets the inverse, i, to inverse
##getinverse <- function() i returns the inverse, i
##ist(set = set, get = get,setinverse = setinverse,getinverse = getinverse) returns the 'special vector' containing all of the functions just defined

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## To summarize, the lexical scoping assignment in R Programming takes advantage of lexical scoping and the fact that functions that return objects of type list() also allow access to any other objects defined in the environment of the original function. In the specific instance of makecachematrix() this means that subsequent code can access the values of x or i through the use of getters and setters. This is how cacheSolve() is able to calculate and store the inverse for the input argument if it is of type makecachematrixr(). Because list elements in makecachematrix() are defined with names, we can access these functions with the $ form of the extract operator.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
