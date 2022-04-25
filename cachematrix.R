

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(new) {
        x <<- new                           #seconding all the definations and alignment of code
        inv <<- NULL    
        
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse  <- function() inv 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
        message("getting cached data")  #getting the data from directory  cache
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) # assigning inv to be solved
        x$setinverse(inv)
        inv
}

#output of inverse is seen 
