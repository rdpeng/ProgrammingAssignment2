## function makeCacheMatrix() creates a matrix object that caches its inverse
## function cacheSolve() returns inverse of a matrix returned by the above listed funtion.
## If the inverse has already been calculated and the matrix has not changed, it'll retrieves the inverse from the cache directly.


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    # get it from the cache and skips the computation
    return(m)
  }
  
  ## otherwise, calculates the inverse 
  matrix <- x$get() 
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
