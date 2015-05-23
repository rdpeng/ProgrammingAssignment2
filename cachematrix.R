makeCacheMatrix <- function(x = matrix()) {
  # m will store the matrix values
  m<-NULL
  # setter for the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  # getter for the matrix
  get<-function() x
  # inverting the matrix
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    # if values are alreadyy available
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}