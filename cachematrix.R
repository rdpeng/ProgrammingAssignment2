## Assignment 2 : Program to compute matrix inverse and cache it

## This function will initialise and return list which involves functions like Set , get , setmean and getmean
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


## function to cache matrix inverse

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
