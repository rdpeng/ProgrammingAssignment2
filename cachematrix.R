## The functions below employ the rules of lexical scoping to create 
## a matrix, compute its inverse and simultaneously cache its value
## for the purpose of expediting its subsequent access 
## in the future.  


## The function below creates an object of a matrix format
## that is capable of caching the inverse of a matrix in memory in order 
## to accelerate its access in the future. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The cacheSolve function returns the inverse of the matrix created 
## with the function above.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
aMatrix <- matrix(c(0.5, -0.25, -1, 0.75), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(aMatrix)
n1 <- cacheSolve(myMatrix_object)

## Checking whether multiplication of matrices aMatrix and 
## its inverse, n1, returns an Identity Matrix.

aMatrix %*% n1
