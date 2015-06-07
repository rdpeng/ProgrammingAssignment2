## makeCacheMatrix creates a Matrix and calculates its inverse 
## if it has not already been calculated. In this case it
## will be retrieved from the Cache


makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y){
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invm <<-inverse
    getinv <- function() invm 
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}

## cacheSolve retrieves the inverse of a given matrix 
## from the Cache. If the inverse is not available,
## the function will calculate and cache it.

cacheSolve <- function(x,...){
    invm <- x$getinv()
    if(!is.null(invm)){
        message("getting inverse matrix from Cache")
        return(invm)
    }else{
        invm <- solve(x$get())
        x$setinv(invm)
        return(invm)
    }
}

