makeCacheMatrix <- function(x = matrix()) {
    #initializing the inverse of the matrix property
    m <- NULL
    
    #Setting of the matrix and set up in a different enviornment with <<- 
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #get the matrix data
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#Compute the inverse of the special matrix returned by "makeCacheMatrix" above. If the inverse has already been calculated 
#(and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        #Return a matrix that is the inverse of 'x'. Inspired from the example in the instructions.
        m <- x$getInverse()
        #if we found a valid data then return it
        if(!is.null(m))
        {
            message("getting cached data")
            return(m)  
        }
        
        #Get the matrix from our object
        data <- x$get()
        
        #Calculate the inverse using matrix multiplication
        m <- solve(data)
        
        #Set the inverse to the object
        x$setInverse(m)
        
        #Return the matrix
        m
}
