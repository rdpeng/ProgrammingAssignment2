# makeCachematrix creates a special matrix-like object and stores it in
## the cache.  
## cacheSolve computes the inverse of the created matrix.  If the inverse
## has already been calculated it will retrieve it from the cache.

## This function creates a "matrix" that is comprised of a list that performs the 
## following steps:  1) sets the matrix value, 2) gets the matrix value, 
## 3) solves for the inverse of the matrix, and 4) gets that inverse.

makeCacheMatrix <- function(x = matrix()) {
  y<-NULL
  set <- function(z){
    x<<-z
    y<<-NULL
  }
  get <- function()z
    solveinv<-function(solve) y<<-solve
    getinv <- function()y
    list (set=set, get=get,
          solveinv=solveinv,
          getinv=getinv)
  }



## The following function solves for the inverse of the above matrix and caches it
## If the inverse has already been solved then it uses getinv to retrieve the 
## inverse, otherwise it uses solveinv to solve for the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    y<-x$getinv()
    if (!is.null(y)){
      message("retrieving cached data")
      return (y)
    }
    data<-x$getinv
    y<-solve(data,...)
    x$setinv(y)
    y
}
