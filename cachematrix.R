## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
        
        if (is.matrix(x) == FALSE) x <- as.matrix(x)
        
        s <- NULL
            
        set <- function(y){
                
                if (is.matrix(y) == FALSE) y <- as.matrix(y)
                
                x <<- y
                s <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(solve) s <<- solve
     
        getinv <- function() s
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        s <- x$getinv()
        
        if(!is.null(s)){
                
                message("getting inverse matrix")
                return(s)
        }
    
        data <- x$get()
        
        s <- solve(data)
        
        x$setinv(s)
        
        s
        
}
