#This function stores the value of an inverse of a matrix 
#in cache memory so computation becomes faster

#Initialize the matrix
#Sets matrix x to new matrix y & sets inverse to null matrix
#get the matrix
#setinv set the inverse to inv
#getinv returns the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        #Initialize the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #get the matrix
        get <- function() x
        
        #set the inverse
        setinv <- function(solve) inv <<- solve
        
        #getting the inverse
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##x is output of the above function
##first checks if the inverse has been calculated earlier
##otherwise calculates inverse
##sets the value of inverse
##returns inverse

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        
        #checking if the mean has been calculated earlier
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        #printing inverse
        inv
}