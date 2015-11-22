## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  varInv<-NULL
  set<-function(y){
    x<<-y
    varInv<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) varInv<<- solve
  getmatrix<-function() varInv
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  varInv<-x$getmatrix()
  if(!is.null(varInv)){
    message("Retrieving cached data")
    return(varInv)
  }
  matrix<-x$get()
  varInv<-solve(matrix, ...)
  x$setmatrix(varInv)
  varInv
}
