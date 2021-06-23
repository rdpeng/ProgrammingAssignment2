## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(matrix){
        m<<-matrix
        i<<-NULL
        }
        get<-function(){
        m
        }
        sinverse<-function(inverse){
        i<<-inverse
        }
        ginverse<-function(){
        i
        }
        list(set=set,get=get,
            sinverse=sinverse,
            ginverse=ginverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$ginverse()
        if(!is.null(m)){
        message("getting cached data")
        return(m)
        }
        data<-x$get()
        m<-solve(data)%*%data
        x$sinverse(m)
        m
}
