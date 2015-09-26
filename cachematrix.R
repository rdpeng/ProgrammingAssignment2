## makeCacheMatrix contains 4 functions :
## get returns the matrix stored in main function
## set changes the matrix stored in main function
## getinv and setinv store the value of the input matrix in a variable inv in the main funtion
## list function stores 4 functions in the makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- (y)
    print (x)
    inv<<-NULL
  }
  get<- function(){x}
  
  setinv<- function (m=matrix()) inv<<-m
  getinv<- function() inv
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve verifies value of inv and if it is not null returns the inverseWrite a short comment describing this function
## if inv is null, then it calculates the inverse of the matrix and returns that as inv

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mdata <- x$get()
  inv <- solve(mdata,...)
  x$setinv(inv)
  inv
}
