## makeCacheMatrix makes a list that set and get a matrix and its inverse in an environment variable.  
## The next function passes the resulting list, calculates and sets the inverse.  With a set inverse, the program uses the cached value.


makeCacheMatrix <- function(x = matrix()) {
        cachedInv <- NULL 
        
        set <- function(userValue = matrix()) {
                x <<- userValue 
                cachedInv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(invVal) {
                cachedInv <<- invVal 
                return(cachedInv)
        }
        
        getInverse  <- function() cachedInv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## check list variable to determine cached inverse and return
## else, solve its inverse and return

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { 
        
        calculatedInverse <- x$getInverse() 
        
        
        if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
                message("Cached data")
                return(calculatedInverse)
        }
        
        matrixToSolve <- x$get()  
        
        calculatedInverse <- tryCatch({ 
                solve(matrixToSolve)
        }, warning=function(w) {
                message("Unresolved result")
                message(w)
        }, error=function(e) {
                message("Error in resolving matrix")
                message(e)
                message("\n")
        })
        
        message("Forcing value of inverse to:") 
        x$setInverse(calculatedInverse)
}
