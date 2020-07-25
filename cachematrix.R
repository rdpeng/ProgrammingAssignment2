## Two functions 'makeCacheMatrix' and 'cacheSolve' are used to store the inverse
## of matirx instead of finding it repeatedly

## This function stores the value of the inverse of matrix and returns it when needed

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
          x<<-y
          m<<-NULL
    }
    get<-function() x
    setInverse<-function(inverse) m<<-inverse
    getInverse<-function() m
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This fuction returns the inverse of given matrix by computing or from cache(if available)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse
    print(m)
    if(!is.null(m)){
          message("Getting catched data")
          return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setInverse(m)
    m
}
