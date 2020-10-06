##These functions first, create a list of functions from which you can: set a matrix, get the matrix you put, 
##set the inverse of the matrix and get the inverse previously calculated, notice that the only function that 
##the user should access is $set(), since it's the only way to change the matrix (or by calling the function again with other argument). 
#The second function, only returns the inverse, to do this first it checks if the argument 
##(which have the matrix and the list of functions), already has an inverse value store at s,
##if not it uses the $setinv() of the argument and passes to it solve(data) which
##contains the inverse of the matrix

## Creates a list of functions and saves the matrix, also can contain the inverse if it has been already provided
makeCacheMatrix <- function(x = matrix()){
  
  s <- NULL
  set <- function(y) {
    if(dim(y)[1]== dim(y)[2]){
      x <<- y
      s <<- NULL}
    else {print("The matrix isn't square")}
  }
  get <- function() x
  setinv <- function(inv) s <<- inv
  getinv <- function() s
  list(set=set,get = get,setinv = setinv,getinv = getinv)}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("Getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}