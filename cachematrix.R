## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ##Initializing inverse property
        m <-NULL
        
        ##Setting the matrix
        set <- function(y){
                x <<- y
                m<<-NULL
        }
        
        ##Getting the matrix
        get <- function() x
                
        ##Setting the inverse of the matrix        
        setInv <- function(inv) m<<-inv
        
        ##Getting the inverse of matrix        
        getInv <- function() m
        
        ##Returning list of methods above
        list(set=set, get=get, setInv=setInv, getInv=getInv)
                
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        ##Returning inverse if already set
        if(!is.null(m)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        ## Calculating the inverse using matrix multiplication
        inv <- solve(data,...)
        ##Setting inverse to the object
        x$setInv(inv)
        inv
        
}
