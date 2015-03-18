## By the means of functions below special "matrix" object is created 
## that can cache its inverse. Matrix inverse is not calculated but retreived
## from cache

## Creating cache matrix and defining its elements as functions for applying
## to inverse matrix

  makeCacheMatrix <- function(x = matrix()) {
## intialize the inverse
  inv<-NULL
## set new value to matrix
  set<-function(y){
## Store a new matrix in "x" instead of "y"
    x<<-y
## Reset matrix inverse to Null since we have new matrix "x"    
  inv<<-NULL
}
## return current value of matrix
  get<-function(){x}
## store the value of calculated inverse
  setInv<-function(solve) {inv<<-solve}
## return the inverse matrix "inv" for the stored matrix
  getInv<-function () {inv}
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Function returns cacheSolve function based on setsolve data from cache
## (instead of recalculating this data)

  cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of "x" in the case it exists
  inv<-x$getInv()
  if(!is.null(inv)){
    message ("getting cached data")
    return(inv)
}
  data<-x$get()
## Calculate the matrix inverse and store it as "inv"
  inv<-solve(data, ...)
## Store the inverse "inv" back to original matrix "x"
  x$setInv(inv)
  inv
}
