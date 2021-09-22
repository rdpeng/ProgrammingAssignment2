## with this function i will get the inverse of tje invertible matrix x


makeCacheMatrix <- function(x = matrix()) {
        inv=NULL
        set= function(y) {
        x <<- y
        inv <<- NULL
}
get=function () x
setinv= function (inverse) inv <<- inverse
getinv= function () inv
list=(set=set, get=get,=setinv=setinv, getinv=getinv)
}

## this second function return the inverse of the original matrix x input to the first function

cacheSolve <- function(x, ...) {
        inv=x$getinv()
        if(!is.null(inv)){
        message ("getting cached data")
        return(inv)
        }
    mat.data=x$get()
    inv= solve (mat.data,...)
    x$setinv(inv)
    return(inv)
}
