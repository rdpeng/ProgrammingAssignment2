## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix
##rather than computing it repeatedly.  This code assumes the matrix supplied
##is always invertible.
##Below are two functions that cache the inverse of a matrix:
##
## 1.  makeCacheMatrix : This function creates a special "matrix"
##object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## 2.  cacheSolve : This function computes the inverse of the special
##"matrix" returned by  makeCacheMatrix  above. If the inverse has
##already been calculated (and the matrix has not changed), then
##cacheSolve  should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the
##solve  function in R. For example, if  X  is a square invertible
##matrix, then  solve(X)  returns its inverse.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
