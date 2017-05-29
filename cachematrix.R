## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a "matrix" object that could cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
                }
        get<-function() x
        setinverse <-function(inver) m<<-inver
        getinverse<-function() m
        list(set=set,get=get,
        setinverse=setinverse,
        getinverse=getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached matrix inverse")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
