makeCacheMatrix <- function(x = numeric()) {     # makeCacheMatrix function takes any sqaure matrix (invertible matrix) as an argument and calculates inverse of square matrix using solve function
    cache <- NULL                                # Initialized cache with NULL    
    setMatrix <- function(newValue) {
        x <<- newValue                           # Assign value of matrix i.e. 'x'
        cache <<- NULL
    }   
    getMatrix <- function() {                    # get value of matrix i.e. 'x'
        x
    } 
    cacheInverse <- function(solve) {            # cacheInverse function calcualtes inverse of matrix 'x' and assign the result to cache
    cache <<- solve                              
    }
    
    getInverse <- function() {                  # getInverse function is used to get inverse of matrix 'x'
        cache
    }
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)    #create a list will all the functions
}
cacheSolve <- function(y, ...) {                # cacheSolve function takes matrix as an argument and checks whether matrix inverse is calculated by makeCacheMatrix function. If it is calculated than it print message by returning a inverse of matrix else calculate Inverse of matrix 'x' and return it
    inverse <- y$getInverse()                   # Retrieve matrix inverse
    if(!is.null(inverse)) {                     # If matrix inverse is already calculated
        message("getting cached data")
        return(inverse)                         # Return a matrix that is the inverse of 'x'
    }
    data <- y$getMatrix()                       #  Retrieve a matrix using get method
    inverse <- solve(data)                      # Calculate inverse of matrix
    y$cacheInverse(inverse)                     # Set value of inverse of matrix
    inverse                                     # Return a matrix that is the inverse of 'x'
}
