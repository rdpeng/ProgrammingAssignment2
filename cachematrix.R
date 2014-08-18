## These functions will return the inverse of a matrix that has been passed to it. 
## It will first attempt to return a cached version of the value if it has been previously
## computed. If the matrix value has not been computed or has been changed it will compute it.

## This function manages the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL ## Set the inverse matrix object to NULL
        
        ## This function sets the initial value of the matrix
        ## and sets the inverse object to NULL

        set <- function(m) { ## m contains the new matrix value 
                
                x <<- m ## Store the new matrix value in x
                
                i <<- NULL ## Set the inverse 
        }
        
        get <- function() x
        
        setinverse <- function(solve) i <<- solve
        
        getinverse <- function() i
        
        list(set = set, get = get,
             
             setinverse = setinverse,
             
             getinverse = getinverse)

}


## This function will test for the existence of a cached value and return it if found
## otherwise it will call the above function to perform the computation required 



cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return (i) ## Return the cached value
        }
        
        data <- x$get() ## Get the new matrix values
        
        i <- solve(data, ...) ## Calculate the inverse matrix value
        
        x$setinverse(i) ## Cache the new inverse value
        
        i ## Return a matrix that is the inverse of 'x'
        
}
