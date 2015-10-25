## The function makeCacheMatrix  makeVector creates a list containing a function to
## set the value of the vector,get the value of the vector,
## set the value of the inverse and get the value of the inverse


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


## The following function calculates the inverse of the matrix
## created with the above function. It first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the
## cache via the setmean function.

cacheSolve <- function(x, ...) {
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
