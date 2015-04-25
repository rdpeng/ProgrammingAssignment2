## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##set()- Set the Matrix
##get()- Get the Matrix
##setatrix()-Set the value of Matrix for Inverse using Solve
##getmatrix()-Get the value of Inverse Matrix after checking if the cache has it already calculated.
makeCacheMatrix <- function(x = matrix()) {
 ## Creates Cache Matrix for 'x'
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x

setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

##Test If function is working correctly.
##D = matrix( c(1,1,1,4,3,3,3,3,4), nrow=3, ncol=3) 
##a$set(matrix(D,3,3))
##cacheSove(a)
