# Create a special matrix object to cache its inverse
# Argument parameter: Matrix
# Output: List
makeCacheMatrix <- function(x = matrix()){
    # Initialize the inverse value
    m <- NULL
    
    # Method to set the matrix
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    # Method to get the matrix
    get <- function() x
    
    # Method to set the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
    
    # Method to get the inverse of the matrix
    getinverse <- function() m
    
    # Output list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Compute the inverse of the special matrix
# Argument parameter: Matrix
#Output: Inverse Matrix
cacheSolve <- function(x, ...){
    # Initialize a matrix that is the inverse of x matrix
    m <- x$getinverse()
    
    # Return a matrix if it is the inverse of x matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Get the matrix from the object
    data <- x$get()
    
    # Method to solve the inverse using matrix multiplication
    m <- solve(data, ...)
    
    # Set the inverse of inverse matrix
    x$setinverse(m)
    
    # Return the matrix
    m
}
