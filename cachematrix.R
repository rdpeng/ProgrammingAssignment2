##This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix())  
{                                            ##define the argument with default mode matrix
        inv <- NULL                          ##initialize inv as NULL
        set <- function(y)                   ##define the set function to assign new
                {                 
                x <<- y                      ##value of the matrix in parent environment
                inv <<- NULL                 ##if there is a new matrix reset inv to NULL
                }
        get <- function() x                  ##define the get function
        
        setinverse <- function(inverse) inv <<- inverse  ##assign value of inv in parent environment
        getinverse <- function() inv                     ##gets the value of inv when called
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Caching inverse of a matrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv))
            {
            message("Getting Cached Data")
            return(inv)
            }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
}


