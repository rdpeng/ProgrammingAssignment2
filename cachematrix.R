##Pair of functions that cache the inverse of matrix
##This function creates special"matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
set<- function(y){
x<<-y
inv<<-NULL
}
get <- function()x
setInverse<- function(solveMatrix) inv<<- solveMatrix
getInverse<-function()inv
list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function compute the inverse of special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
}
        data <-x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
        }

