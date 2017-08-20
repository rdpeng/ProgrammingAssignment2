## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv<<-NULL
  Set<-function(Y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function ()x
  setinv<- function(inv)
  {inverse<- inv}
  getinv<- function()inv
  list(set=set,get=get,setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
## Check if the determinant of the matrix is zero
  if(det(data)==0)
  {
    print("can't get inverse")
  }
  else
  {
  inv<- solve(data, ...)
  x$setinv(inv)
  inv
  }
        ## Return a matrix that is the inverse of 'x'
}
