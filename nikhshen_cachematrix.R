@@ -2,14 +2,39 @@
## functions do

## Write a short comment describing this function
## the below function sets a cache variable for storing inverse of the input matrix 
## it also has function for getting input matrix and getting its inverse and setting the inverse
## the function returns a list

makeCacheMatrix <- function(x = matrix()) {

  InverseMatrix <- NULL
  set <- function(y) {
        InverseMatrix <<- NULL
        x <<-y
  }
  get <- function () x
  setinverse <- function(inverse) InverseMatrix <<- inverse
  getinverse <- function () InverseMatrix
  list (set= set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
#below function checks if inverse of input matrix is avaialble or not
#if not available it triggers the get function of "makeCacheMatrix"
##function and sets inverse of matrix into 'InverseMatrix' variable
## next time when the function is called again, the value is fetched from the 
##cached variable 'InverseMatrix'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InverseMatrix <- x$getinverse()
  if (!is.null(InverseMatrix)){
    message ("getting the inverse of the matrix from matrix")
    return (InverseMatrix)
  }
  data <- x$get()
  InverseMatrix <- solve (data,...)
  x$setinverse (InverseMatrix)
  InverseMatrix
}