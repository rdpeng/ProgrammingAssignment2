## a pair of functions that cache the inverse of a matrix

## this function creates a special "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
                }
        get<-function()x
        setinv<-function(solveMatrix) inv<<-solveMatrix
        getinv<-function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## this function computes the inverse of the special "matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                }
        data<-x$get()
        inv<-solve(data)
        x$setinv(inv)
        inv
}
