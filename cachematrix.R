## mkeCacheMatrix creates a cachable matrix object.
## cacheSolve solves or retrives

## Creates matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        setmatrix<-function(y) {
        x<<-y
        m<<-NULL
        }
        getmatrix<-function()x
        setinv<-function(inverse) m<-inverse
        getinv<-function() m
        list (setmatrix=setmatrix,getmatrix=getmatrix, 
        setinv=setinv,getinv=getinv)
}


## Solves the inverse; if the inverse has already been calc'd, 
## will retrive the cached matrix

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
        message("retreiving cached matrix")
        return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinv(m)
        return(m)
}
