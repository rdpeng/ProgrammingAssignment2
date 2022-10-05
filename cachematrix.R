## Put comments here that give an overall description of what your
## functions do
## we use 2 functions namely makeCacheMatrix and cacheSolve
## makeCacheMatrix consists of set,get,setinv,getinv
## library(MASS) is used to calculate the inverse for non square as well as square matrices too
## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL                              ##initialising inverse matrix to Null
  set<-function(y){
                  x<<-y
                  inv<<-NULL
                  }
  get<-function()x     ##function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)              ##function to obtain inverse of matrix
                    inver%*%x
                    }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
##This is used to get cache data
cacheSolve <- function(x, ...) {
                                                 ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
  if(!is.null(inv)){                             ##check whether the matrix is null or not
                    message("getting cached data!")
                    return(inv)                  ##returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)    ##calculates inverse value
  x$setinv(inv)
  inv    ##return a matrix that contain the inverse of 'x'
}
