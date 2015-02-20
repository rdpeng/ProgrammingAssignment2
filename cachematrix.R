# objective : pair of functions that cache the inverse of a matrix.
# set the value of matrix
# get the value of matrix
makeCacheMatrix <- function(x = matrix()) {
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

# calculates the inverse of the  matrix, checks to see if the inverse of the matrix is already calculated
# if the inverse is already calcualted, it gets it from the cache
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

#The below lines are for testing
#test <- matrix(data = c(4,3,3,2), nrow = 2, ncol = 2)
#test1 <- makeCacheMatrix(test)
#cacheSolve(test1)
