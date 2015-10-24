## Below are two functions that are used to create a special object 
## that stores a numeric matrix and caches its mean

## The first function is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL

  setMatrix <- function(y){
    x <-- y
    m <-- NULL
  }
  getMatrix <- function() x
  setInvMatrix <- function(InvMatrix) m <<- InvMatrix
  getInvMatrix <- function(InvMatrix) m 
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## The second function calculates the inversed matrix of the special
## "matrix" created with the above function. However, it first checks
## to see if the inversed matrix has already been calculated. If so,
## it gets it from the cache and skips computation. Otherwise, it
## calculates the inversed matrix of the data and sets the value of 
## the inversed matrix in the cache via the setInvMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting the cached inversed matrix")
    return(m)
  }
  m <- solve(x$getMatrix())
  x$setInvMatrix(m)
  m
}
