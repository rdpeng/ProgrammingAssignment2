## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

  ### 1st --> initialise the inverse property
  ### 2nd --> method to set the function where m is the matrix
  ### 3rd --> method to get the matrix 
  ### 4th --> method to return the matrix
  ### 5th --> method to set the inverse
  ### 6th --> return a list of methods

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  get <- function() {
    m
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

  ### Computes the inverse of the matrix created above

### 1st --> return a matrix that is the inverse of x
### 2nd --> return the inverse if it is already set
### 3rd --> get matrix from object
### 4th --> get hte matrix from the object
### 5th --> calculate the inverse using matrix multiplication
### 6th --> set the inverse to the object
### 7th --> return the matrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %% data
  x$setInverse(m)
  m
}


