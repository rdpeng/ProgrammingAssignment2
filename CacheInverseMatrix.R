mat <- matrix(c(1:4),c(2,2))

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y){
                x <<- y 
                i <<- NULL  
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)    
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("Retreiving the cached inverse matrix...")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinv(i)
        i
}