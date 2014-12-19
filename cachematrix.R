## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    invMat <- NULL
  
    set <- function(y){
        x<<- y
        invMat <<- NULL
    }
  
    get <- function() x
  
    setInvMat <- function(inverse) invMat <<- inverse
  
    getInvMat <- function() invMat
  
    list(set=set, get=get, setInvMat=setInvMat, getInvMat=getInvMat)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
    inv<-x$getInvMat()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInvMat(inv)
    inv
}







