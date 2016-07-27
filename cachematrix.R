## The first function, makeCacheMatrix creates a special "matrix", which is really
## a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(inv) m<<-inv
  getinv<-function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## The following function calculates the mean of the special "matrix" created with
## the above function. It first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setinv function. 

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
matrix<-x$get()
m<-solve(matrix, ...)
x$setinv(m)
m    ## Return a matrix that is the inverse of 'x'
}
