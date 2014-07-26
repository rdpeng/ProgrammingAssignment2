## [This code creates two functions that create a special matrix object and
## calculate its inverse respectively]

## Creates a special matrix object with methods (functions) to get the matrix,
## set its value, get its inverse matrix, and set it
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(vector,dim){
    x<<-matrix(vector,dim,dim)
    i<<-NULL
  }
  get <- function() x
  setsolve <- function (inverse) i<<-inverse
  getsolve <- function() i
  
  list (set=set,get=get,setsolve=setsolve,getsolve=getsolve)
  
}


## computes the inverse of the matrix created with makeCacheMatrix if
## it has not been solved (stored) already. If the inverse exists, this 
## function returns it (does not compute it) with a message.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$getsolve()
  
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setsolve(i)
  i
}
