## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeVector creates a special "vector", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) i <<- invert
  getinvert <- function() i
  
  
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
 
}


## Write a short comment describing this function
#The following function calculates the mean of the special "vector" created with the above function. 
#However, it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinvert function

# So, to test any square matrix 'm'; call -> cacheSolve(makeCacheMatrix(m))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinvert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvert(i)
  i
}


