

## Write a short comment describing this function:

##makeCacheMatrix is a list containing functions to 
##set value of the matrix, 
##get value of the matrix, 
##set value of the inverse
##get value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##empty object
  set <- function(y){
    x <<- y ##assign variable within the "set" function (enclosed environment)
    m <<-NULL ##empty object
  } ##sets the value of the matrix
  get <- function(){x} #get (print) the value of the matrix 
  setInverse <- function(inverseMatrix){m <<- inverseMatrix} #sets the value of inversed matrix
  getInverse <- function(){m} #get (print) the value of the inverse matrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
} ##makes a "cache matrix" where inversion can be stored


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse() ##assign/use getInverse function
  if (!is.null(m)){ 
    message("getting cached data")
    return(m)
  } ##checks if the inverse has been previously calculated and skips calculation if so
  
  ## if not calculated, then perform the calculation 
  mat_rix = x$getInverse()
  m <-solve(mat_rix) ##calculate inversion using result of getInverse
  x$setInverse(m) ##sets the value of the inversed matrix in the cache via "setInverse"
  m ##get/print calculated matrix
}