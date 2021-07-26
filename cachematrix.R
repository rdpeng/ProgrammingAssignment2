## The function will find the inverse of the matrix created by the first few lines
## and if the inverse of this matrix has been found beforehand and there is no changes
## it will return the value(i.e matrix) to save some time.

## This function is a starting point to assign and show value of the specified matrix

makeCacheMatrix <- function(x = matrix()) {
    set<-function(y=matrix()){
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    set_inv<-function(I) inv<<-I
    get_inv<-function() inv
    list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}


## This part of the function returns the inverse of a matrix by firstly checking for 
## the prespecified value of this particular matrix

cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inv(inv)
    inv
}
