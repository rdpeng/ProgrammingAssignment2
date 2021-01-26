## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                       
                        invm <- NULL
                        set <- function(y = matrix()) {
                                x<<- y
                                invm <<- NULL
        
                        }
                        get <- function() {x}
                        setinverse <- function(solve) {invm <<- solve}
                        getinverse <- function() {invm}
                
                        list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
                        
}

## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
             
        invm <- x$getinverse()
                if(!is.null(invm)) {
                        message("getting cached data")
                        return(invm)
                        
                }
                dt <-x$get()
                invm <-solve(dt, ...)
                x$setinverse(invm)
                invm
        
        ## Return a matrix that is the inverse of 'x'
}
