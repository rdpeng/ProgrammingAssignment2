## Put comments here that give an overall description of what your
## functions do

## This function allows the user to set a square matrix and then :
          ##1) set a value for the matrix (if not done at the level of the function) 
          ##using the set() function
          ##2) print the matrix with the get() function
          ##3) set an inverse to the matrix with the setsolve() function
          ##4) print the inverse of the matrix set in the previous function.
          ##If the user didn't specify any value with the setsolve() function,
          ##the value of getsolve() will be "NULL"

##Example : set my_matrix<-makeCacheMatrix(x=matrix(1:4, 2, 2))

##For using get() function, do my_matrix$get()
##For setting a standard inverse to the matrix, do my_matrix$setsolve(matrix(c(-2, 1, 1.5, -0.5), 2, 2))
##For printing the inverse set, do my_matrix$getsolve() ##this will print NULL if the user did not set a standard inverse


makeCacheMatrix<- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  ##set a value for the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x   ## Print the value of the matrix
  setsolve <- function(solve) m <<- solve    ##set an inverse to the matrix
  getsolve <- function() m   ##print the inverse of the matrix, if defined (otherwise, NULL)
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function will do two things :
          ##1) If the user defined an inverse in the previous function using the 
          ##setsolve() function, if will print the value directly form the cache
          ##2) If the user did not specify any particular inverse value (i.e. when 
          ##getsolve() returns NULL, the function will calculate the inverse of
          ##squared function defined with te set() function, and will print it

## Example :  *If a standard inverse was set, cachesolve(my_matrix) will print 
##            the message "getting cached data", and will print the inverse matrix set
##            *If no inverse was set by the user previously, cachesolve(my_matrix) 
##            will calculate the inverse of the squared matrix, and return its value

cacheSolve<- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
