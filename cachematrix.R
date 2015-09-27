## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setinv=function(ginv)m<<-ginv
        getinv<-function()m
        list(set=set,
        get=get,
        setinv=setinv,
        getinv=getinv)
}
## Return a list of set/get/setinv/getinv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$igetinv()
        if(!is.null(m)){
        message('getting cached data')
        return(m)
        }
        data<-x$get()
        m<-ginv(data,...)
        x$setinv(m)
        m
}
