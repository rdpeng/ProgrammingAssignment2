## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix(rnorm(16),4,4)) {
  inv <- NULL
  b <- x
  get <- function() b
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  cacheSolve <- function(a){
    ## Return a matrix that is the inverse of 'x'
    linv <- a$getInverse()
    if(!is.null(linv) && a$get()==b){
      message("getting cached inverse")
      return(linv)
    }
    linv<-solve(a$get())
    a$setInverse(linv)
    linv
  }
  list(cacheSolve=cacheSolve, get=get, setInverse=setInverse, getInverse=getInverse)
}

