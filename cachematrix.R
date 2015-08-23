makeCacheMatrix <- function(x = matrix()){
        cacheInv <- NULL
        
        set <- function(userValue = matrix()){
                x <<- userValue
                cachedInv <<- NULL
        }
        
        get <- function() x

        setInverse <- function(invVal){
                cacheInv <- invVal
                return(cacheInv)
        }
        
        getInverse <- function() cachedInv
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## given the list variable from the first function, will first check to see if there's already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...){
        
        ## let's see if there's something there already
        calculatedInverse <- x$getInverse()
        
        ##check if there's a cached value AND it's a matrix
        if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)){
                message("We found cached data and saved valuable cpus!!!")
                return(calculatedInverse)
        }

        #else get the Inverse matrix 
        matrixToSolve <- x$get()
        
        calculatedInverse <- tryCatch({ 
                #to get the matrix
                solve(matrixToSolve)
        }, warning=function(w) {
                message("This may not be the result you're looking for")
                message(w)
        }, error=function(e) {
                message("Something went wrong solving your matrix")
                message(e)
                message("\n")
        })
        
        ## whatever the case, set the value of the inverse (NULL if something went wrong)
        message("Setting the value of inverse to:") 
        x$setInverse(calculatedInverse)
        

}
