## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse, 
# which is a list containing a function to:
# 1 - Set the value of the matrix
# 2 - Get the value of the matrix
# 3 - Set the the inverse of the matrix
# 4 - Get the the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
    # Sets the value of the "i" variable to a default value
    i <- NULL
    
    # Definitions of the functions that are contained in the list returned by the function
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    
    # Returns the list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    
    # Sets the value of the "i" variable to the inverse of the matrix passed to the function "makeCacheMatrix"
    i <- x$getinv()
    
    # If the inverse of the matrix has not calculated yet, then ignores this "if" statement
    if(!is.null(i)) {
        # If the inverse of the matrix has calculated already, then prints a message and returns the result
        message("getting cached data")
        return(i)
    }
    
    # Gets the matrix, calculate the inverse of that matrix and sets the result to the object
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    
    # Returns the inverse of the matrix
    i
}