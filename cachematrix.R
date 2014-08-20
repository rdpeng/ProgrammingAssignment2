

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  my_matrix <-NULL
  set<-function(param){
  x<<-y
  my_matrix <<-NULL
}
get<-function() x
setmatrix<-function(solve) my_matrix<<- solve
getmatrix<-function() my_matrix
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    my_matrix<-x$getmatrix()
    if(!is.null(my_matrix)){
      return(my_matrix)
    }
    matrix<-x$get
    my_matrix<-solve(matrix, ...)
    x$setmatrix(my_matrix)
    my_matrix
}
