#Compute the inverse of given matrix and cache the result. If matrix is not changed and inverse is required
# again then inverse should be fetched from cache and should not be recalculated.
# Sample input and output :-
# mat <- matrix (10:13,2,2)
# x<- makeCacheMatrix(mat)
# cacheSolve(x)
#      [,1] [,2]
#[1,] -6.5    6
#[2,]  5.5   -5

## This function creates a special "matrix" object that can cache its inverse
#  Input: Matrix. We assume that the matrix supplied is always invertible.
#  Output: A list containing a function to
#       set the value of the matrix
#       get the value of the matrix
#       set the value of the inverse
#       get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        # The variable "i" is NULL to indicate that we have not 
        # calculated the cached inverse yet.
        i<- NULL
        
        set <- function(y) {
                # setting the value of variables present in  parent environment i.e. makeCacheMatrix
                # When underlying matrix is changed then cache is set to NULL
                x <<- y
                i <<- NULL
               
        }
        
        # Declare a function to return the underlying matrix "x".
        get <- function() x
        
        # Declare a function to cache the inverse of matrix "x" in
        # the variable i present in parent environment
        setinverse <- function(inverse) i <<- inverse
        
        # Declare a function to return the cached inverse of  matrix "x".
        getinverse <- function() i
        
        # Return a list object that contains the function objects we have just defined.
        # Note that the function objects have an environment associated with them        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
#retrieves the inverse from the cache.
#Input: List returned by makeCacheMatrix
# Output: Inverse matrix
cacheSolve <- function(x, ...) {
        
        # check if the inverse matrix is cached and is not NULL
        i <- x$getinverse()
        if(!is.null(i)) {
                
                # Inverse matrix is cached and is stored in i so return i as output
                message("getting cached data")
                return(i)
        }
        
        # Inverse is not cached and should be calculated. Retreive matrix input and store in data
        data <- x$get()
       
        # Calculate inverse using solve function and store output in i
       i <- solve(data, ...)
       
       # Cache new inverse using setinverse function
        x$setinverse(i)
       
       # return inverse which is stored in i
       i
}
