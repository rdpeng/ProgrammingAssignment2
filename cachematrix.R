## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   xinv <- NULL 
        set <- function(y) {
                x <- y
                xinv <- NULL 
        }
        
        get <- function() x 
        setInv <- function(inv) xinv <<- inv 
        getInv <- function() xinv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      m <- x$getInv() 
        
        if(!is.null(m)) {
                return(m) 
        }
        data <- x$get() 
        m <- solve(data) 
        x$setInv(m) 
        m 
}
