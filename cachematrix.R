## Put comments here that give an overall description of what your
## functions do

## This function is used to create a matrix and find its inverse 

makeCacheMatrix<- function(x = matrix()){
  inv<-NULL ##creating inv as NULL matrix
  set<- function(y) {
    x<<-y
    inv<<-NULL
  }
  get <- function() x ##function to get x matrix
  setinv<-function(inverse) inv<<-inverse
  getinv<-function(){
    inver<-ginv(x) ## function to get inverse of matrix x
    inver%*%x
  }
  list (set = set, get = get,
        setinv=setinv,
        getinv = getinv)
}

## This is used to get cached data
cacheSolve <- function(x,...){ ## gets cached data
  inv<-x$getinv()
  if(!is.null(inv)){
    message ("getting cached data")
    return(inv)
  }
    data<-x$get()
    inv<-solve(data,...) ## calculates inverse value
    x$setinv(inv)
    inv ## returns a matrix that is inverse of x
}