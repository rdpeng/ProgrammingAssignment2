## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 n <- NULL
  set <- function(y) {
          x <<- y
          n <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) n <<- inverse
  getinv <- function() n
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##This function takes in an intialised matrix and checks the "list" of functions for its inverse.
##Should it exist, the function retrieves the cached inverse.
##Else, it prints the inverse and caches it for further use. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  n <- x$getinv()
  if (!is.null(n)) {
          message("getting cached data")
          return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinv(n)
  n
}
##An example1
##s = rnorm(4, 5, 4)
##q = matrix(s, nrow = 2)

##c = makeCacheMatrix(q)
##cacheSolve(c)
##This is the first call to invert the matrix
##           [,1]       [,2]
##[1,] -0.6614635 0.05408139
##[2,]  0.5518796 0.07254096

##The second time
##getting cached data
##           [,1]       [,2]
##[1,] -0.6614635 0.05408139
##[2,]  0.5518796 0.07254096
