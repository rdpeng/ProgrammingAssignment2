## makeCacheMatrix creates a matrix object that can cache its inverse,
## in addition, it contains four functions

# get: returns the vector stored
# set: changes the vector stored
# setinverse: store the value of the input in a variable m
# getinverse: returns the value described above in setinverse

makeCacheMatrix<-function(x=matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<- solve
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve computes the inverse of the matrix,

cacheSolve<-function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}

## Example with a random matrix

rm<-diag(2,4)
rm

cmatrix<-makeCacheMatrix(rm)
cacheSolve(cmatrix)