## makeCacheMatrix create a list of functions:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse (solve)
## get the value of the inverse (solve)


makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        ## function to set the matrix using <<- as x & m are outside the scope
        
        set <- function(y) 
                {
                x <<- y
                m <<- NULL
                }
        
        get <- function() x
        
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        
        ## create the list with names = variable used
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
        
}

## cacheSolve get the "getInverse" variable (that's a function)
cacheSolve <- function(x, ...) 
{
        ## retrieve the getInverse variable
        m <- x$getInverse()
        
        ## if is not null return it
        ## it means data are already cached
        if(!is.null(m)) 
                {
                message("getting cached data")
                return(m)
                }
        
        ## if is null retrieve the matrix 
        data <- x$get()
        
        ## calculate the inverse
        m <- solve(data, ...)
        
        ## store the inverted matrix
        x$setInverse(m)
        m
}

