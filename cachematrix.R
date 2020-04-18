## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.
## makeCacheMatrix function computes the inverse of the matrix

makeCacheMatrix<-function(x=matrix()) {
    m<-NULL
    ## set the value of the matrix
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    ## get the value of the matrix
    obt<-function() x
    ## set the value of the inverse
    setinv<-function(inverse) m<<-inverse
    ## get the value of the inverse
    obtinv<-function() m
    list(obt=obt, set=set,
         setinv=setinv,
         obtinv=obtinv)
}


## Write a short comment describing this function
## cacheSolve function retrieve the inverse from the makeCacheMatrix function

cacheSolve<-function(x, ...) {
    m<-x$obtinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data<-x$obt()
    m<-solve(data, ...)
    x$setinv(m)
    m
}
