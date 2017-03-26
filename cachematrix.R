
## Welcome friend!


makeCacheMatrix <- function(x = matrix()) {
 ## Initialize the cache Matrix (cMatrix) and set to NULL
 ## Define function setcMarix to assign the input matrix
 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
  ## Return the input matrix "x" 
  ## Using solve function get the inverse of the cMatrix and Return the inversed matrix of "x"
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
         
 ## List the assigned matrices with the names 
         
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## The below function checks if the given input matrix is already inversed. 
## If not, it then computes the inverse of the input matrix



cacheSolve <- function(x, ...) {
        
        ## Check if the input matrix is already inversed into the cache matrix
        ## If so, it returns value from the cached matrix, else moves past the below "if" statement
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
 ## Gets the cached Matrix and using "Solve" function it inverses the input matrix
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

 ## Thanks for Evaluating my program friend
 ## Have a nice time !
