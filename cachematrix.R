## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #sets the value of the vector
        set <- function(y){
                x < - y 
                m <- NULL
                }
        #get the value of the vector
        get <- function() x
        #set the value of the inverse matrix
        inv <- function(inverse) m <- inverse
        #get the value of the inverse matrix
        getinv <- function()m
        list(set=set, get=get,inv=inv, geinv=getinv)      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.na(m)){
                message("getting cache data")
                return(m)
        }
        data <- m$get()
        m <- solve(datamatrix)
        x$setinv(m)
}
