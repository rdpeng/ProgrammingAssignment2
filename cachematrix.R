## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <-NULL
        set <- function(y){
                x <<- y
                m<<-NULL
        }
        get <- function() x
        setInv <- function(inv) m<<-inv
        getInv <- function() m
        
        list(set=set, get=get, setInv=setInv, getInv=getInv)
                
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data,...)
        x$setInv(inv)
        inv
        
}
