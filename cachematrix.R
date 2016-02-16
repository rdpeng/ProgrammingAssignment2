## The function below creates a list of 4 functions that work on matrices
##      set             - directly (without having to call makeCacheMatrix) sets the matrix & wipes out any cached Inverse 
##      get             - returns the matrix
##      setInverse      - creates the inverse matrix - only cacheSolve should use this.  It shoud not be called directly
##      getInverse      - returns the matrix's inverse or null if cacheSolve has not been run

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) 
        {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        
        setInverse <- function(z) inverseMatrix <<- z
        getInverse <- function() inverseMatrix
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function below calculates the matric Inverse only if the cached Inverse is null
## To determine "null-ness", it uses the getInverse from makeCacheMatrix to get the cached Inverse value 
## If the cached Inverse value is null, it uses get and setInverse from makeCacheMatrix to put the inverse into the cache 

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) 
        {
                message("getting cached inverse")
                return(inverseMatrix)
        }
        data <- x$get()
        
        inverseMatrix <- solve(data) %*% data
        x$setInverse(inverseMatrix)
        
        inverseMatrix
}
