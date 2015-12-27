## Put comments here that give an overall description of what your
## functions do

# R doc edited by Rich Gonzales

# 1 Write function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {    
  m <- NULL                ## sets value of m to NULL inside the function
  set <- function(y) {          ## 1 function to set value of x matrix to value of y (also a matrix)
    x <<- y                     
    m <<- NULL                 ## reset m to NULL
  }
  
  get <- function() x          ## 2 makes a function that returns the new matrix x
  setinverse <- function(solve) m <<- solve           ## 3 create the inverse using solve
  getinverse <- function() m                   ## 4 function returns the matrix inverse in the variable m
  
  list(set = set, get = get,       
       setinverse = setinverse,
       getinverse = getinverse)
}

# #2 Compute the inverse of the cached matrix or if none exists, create the new matrix using logic from MakeCacheMatrix

cacheSolve <- function(x, ...) {  
  m <- x$getinverse()             ## uses getinverse function to get the inverse matrix value and puts in variable m
  if(!is.null(m)) {                    ## checks to see if m variable is not NULL (has previously stored (cached) matrix)
    message("getting cached data")         ## if the not NULL is determined to be true, provides message of impending output
    return(m)                   ## returns the output of the cached matrix
  }
  data <- x$get()          ## if variable m is NULL - get the matrix and put into variable 'data'
  m <- solve(data, ...)     ## use solve function to get inverse of inverted data
  x$setmatrix(m)            ## use the setmatrix function to set m to the new matrix value
  m
}




## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
}
matrix(12)

# Rich: makeCacheMatrix is an object that contains 4 functions to: set the value of a matrix, 
# retrieve the value of the matrix, create an inverse matrix, and return the inverse matrix
# Note that variables outside of the local environment get updated with new values (variables in the global environment)

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
}

# Rich: cacheSolve is an object that checks to see if there is a cached inverse matrix (!is.null) , inverts the matrix, and puts the matrix in a variable. 
# If no cached matrix exists (NULL), then uses the functions from MakeCacheMatrix to create one and cache it.







