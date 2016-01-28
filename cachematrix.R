## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  if(!identical(dim(x)[1],dim(x)[2]))
    stop("The argument must be a square matrix")
  inv <- NULL
  setInverse<-function(mat) inv<<-mat 
  getInverse<-function()  inv
  get<-function() x
  list(setInverse=setInverse,getInverse=getInverse,get=get)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv))
  {
    print("getting cached data...")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat)
  x$setInverse(inv)
  inv
}
