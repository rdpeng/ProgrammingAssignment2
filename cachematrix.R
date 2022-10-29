## A pair of functions that cache the inverse of a square invertible matrix.
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #create a NULL variable that has a value of NULL
  set <- function(y) { #set the value of the matrix
    x <<- y #
    i <<- NULL
  }
  get <- function() x  #get the value of the matrix
  setinv <- function(solve) i <<- solve(x)  #set the value of the inverse and store it in i
  getinv <- function() i  #get the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data") #if the value of i is not NULL print the message
    return(i) #and return the value of i i.e. the inverse stored in i
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

# create a square invertible matrix
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

#test the code
A1 <- makeCacheMatrix(A)
cacheSolve(A1)
cacheSolve(A1)
