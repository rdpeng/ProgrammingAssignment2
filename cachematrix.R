## Put comments here that give an overall description of what your
## functions do

## Write 
##a short comment describing this function







## This function creates a special "matrix" object that can cache its inverse.



cacheSolve <- function(x, ...) {
      makeCacheMatrix <- function(x = matrix()) {
   theInv<-NULL
   set<-function(y){
      x<<-y
      theInv<<-NULL
    }
    get<-function() x
    setmatrix<-function(cacheM) theInv<<- cacheM
    getmatrix<-function() theInv
    list(set=set, get=get,
    setmatrix=setmatrix,
    getmatrix=getmatrix)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
    theInv<-x$getmatrix()
    if(!is.null(theInv)){
      message("getting Cache Matrix")
      return(m)
    }
    matrix<-x$get()
    theInv<-solve(matrix, ...)
    x$setmatrix(theInv)
    theInv
}



## Return a matrix that is the inverse of 'theInv'
}
