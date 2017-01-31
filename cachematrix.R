## Put comments here that give an overall description of what your
## functions do

## This function creates the functions needed to set and get the 
## inverse functions to be applied to a square matrix

## base function ensures that arg 'x' is a square matrix
makeCacheMatrix <- function(x = matrix()) { 
     ## set variables within the function environment to
     ## store the matrix
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     ## define function to get the inverse values of 
     ## the matrix, 'x'
     ## ## Gets the original matrix
     get <- function() x  
     ## inverse matrix function
     setinv <- function(solve) m <<- solve
     # apply the inverse matrix function setinv on matrix
     getinv <- function() m  #
     ## Return each function as a list so they can be used
     ## later in the global environment
     list(set = set, 
          get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function will check if the inverse of the matrix exists in
## cache and if not, calculate and store the inverse to cache and
## return the inverse to the console.  If inverse exists in cache,
## that will be returned to the console without calculation

## Base function call
cacheSolve <- function(x, ...) {
     ## Retrieve the matrix stored in x$getinv to 'm'
     m <- x$getinv()
     ## If m is not null, inverse is already existing in cache
     ## and so return it and stop processing.  Display a message
     ## during processing to let user know something is happening
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     ## In case the inverse matrix is not stored in cache
     ## Calculate the inverse, store the result to cache
     ## and return the result to the console
     ## Get the data and store to variable 'data'
     data <- x$get()
     ## Apply function solve to data to invert the matrix
     m <- solve(data, ...) 
     ## Apply the result 'm' to x$setinv to store in cache
     x$setinv(m) 
     ## print the result 'm' to the console
     m
}

### Create three 2x2 Test Matrices
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)

## Test the functions....
m1                       ## Print m1 to the console
a <- makeCacheMatrix(m1) ## Cache m1 matrix
cacheSolve(a)            ## invert m1 matrix
cacheSolve(a)            ## Run cacheSolve again to ensure it gets results from cache
solve(m1)                ## Validate results are correct

I2                       ## Print I2 to the console
a <- makeCacheMatrix(I2) ## Cache I2 matrix
cacheSolve(a)            ## invert I2 matrix
cacheSolve(a)            ## Run cacheSolve again to ensure it gets results from cache
solve(I2)                ## Validate results are correct

n1                       ## Print n1 to the console
a <- makeCacheMatrix(n1) ## Cache n1 matrix
cacheSolve(a)            ## invert n1 matrix
cacheSolve(a)            ## Run cacheSolve again to ensure it gets results from cache
solve(n1)                ## Validate results are correct

