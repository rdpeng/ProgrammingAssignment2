## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to store the inverse of the special matrix
 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the matrix
# 4.  get the value of the matrix

# The matrix will be always a square matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv<- NULL
  
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
#This Function take the storage Matrix and find the Inverse of the matrix
#from the below function.


  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
 
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
  
  

