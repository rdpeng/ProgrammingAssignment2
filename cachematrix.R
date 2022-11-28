## These functions together are used to save time and computational resources by caching the
## results of a matrix inversion

## Per the assignment, this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<- function() x
    setInverse<-function(inv) m<<-inv
    getInverse<-function() m
    list(set=set,get=get,
         setInverse=setInverse, 
         getInverse=getInverse)
} 


## Per the assignment, this function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        myMatrix<-x$get()
        m<-solve(myMatrix)
        x$setInverse(m)
        m
}
