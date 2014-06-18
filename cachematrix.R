# Creates a special function, makeCacheMatrix that can cache its inverse

makeCacheMatrix <- function( mx = matrix() ) {
        
        # Initialize the inverse property of matrix
        inv <- NULL
        
        # Set the value of matrix
        set <- function( mz ) {
                mx <<- mz
                inv <<- NULL
        }
        
        # Method to get the matrix
        get <- function() {
                # This return the matrix
                mx
        }
        
        # Way to set the inverse of the matrix
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        
        # Way to get the inverse of the matrix
        getInverse <- function() {
                #Inverse property is return
                inv
        }
        
        # Return a list of all the above functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# The following function compute the inverse of the special matrix returned by
#"makeCacheMatrix". If the inverse has already been calculated, it gets the 
#inverse from the cache and skips the computation. If not, then it calculate the
#inverse of the matrix & sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        
        # Check the inverse if it's already cached
        inv <- x$getInverse()
        
        #If so, get the inverse from cache directly
        if( !is.null(inv) ) {
                message("getting cached data")
                return(inv)
        }
        
        # Else, we first get the matrix
        data <- x$get()
        
        # Calculate the inverse
        inv <- solve(data, ...)
        
        # Set the inverse of the matrix
        x$setInverse(inv)
        
        # Finally, return the result
        inv
        
}
