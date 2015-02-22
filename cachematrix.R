# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL 
  setmatrix<-function(y){ 
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
# cacheSolve returns the inverse of the matrix. If the inverse has already
# been computed, it gets the result and skips the computation. If not,
# it computes the inverse and sets the value in the cache via setinverse function.
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
