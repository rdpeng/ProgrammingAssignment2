## two functions below passing a value from one function to another 
## using R scoping rules

## the makeCacheMatrix function receives square matrix as input 
## assigns inverse of this matrix (calculated in cacheSolve function)
## and returns list of functions used in another function

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(y){
    x <<- y
    m_inv <- NULL
  }
  get = function() x
  setinv = function(inverse) m_inv <<- inverse
  getinv = function() m_inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## the cacheSolve function uses makeCacheMatrix function to see if the
## inverse matrix has been created for the square matrix passed to that function
## if the inverse has already been created it prints a message and returns the inverse matrix back
## if not, the cacheSolve function gets the original square matrix from the makeCacheMatrix function
## creates and sets inverse matrix and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if(!is.null(inv)) {
    message("getting chached data")
    return(inv)
  }
  mx.data <- x$get()
  inv = solve(mx.data,...)
  x$setinv(inv)
  return(inv)

}
