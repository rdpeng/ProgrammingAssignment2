## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(matlib)
makeCacheMatrix <- function(x = matrix()) {
    if (ncol(x)==nrow(x) && det(x)!=0) {
        m<-NULL
        set<-function(y){
            x<<-y
            m<<-NULL
        }
        get<-function() x
        setinverse <- function() m <<- inv(x)
        getinverse<-function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
    }else{
        return(message("The matrix is'n invertible."))
    }
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data<-x$get
    m <- inv(data, ...)
    x$setinverse(m)
    m
}

x<-makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3))
x$get()
x$getinverse()