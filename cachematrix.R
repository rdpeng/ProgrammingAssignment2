## These two functions create a matrix and calculate its inverse. The result is
## stored in the special matrix object created by 'makeCacheMatrix' so that it 
## can be returned without further computation if it has allready been calculated

## makeCacheMatrix creates a special 'matrix' object that can be passed to 
## 'cacheSolve' to solve the inverse of the matrix object. The object can 
## be used to set the value of the matrix, get the value of the matrix
## and also cache its inverse (setinv) and return the value of a cahced 
## (getinv) inverse.

makeCacheMatrix <- function(x = matrix()) {
        # First check that input was a matrix
        if (!is.matrix(x)) {
                stop("input was not a matrix")
        }
        
        inv <- NULL # Set the inverse to NULL
          
        # Function to set the value of our matrix and also overwrite a cached
        # inverse if it exists
        set <- function(y){
                x <<- y #when set is called, set x to be the variable passed to "set"
                inv <<-NULL #set inverse to NULL since our matrix has now changed
        }
        
        # Function to return matrix value 
        get <- function() x
        
        # Function to set the inverse value
        setinv <- function(z) inv <<- z 
        
        # Function to return cached inverse
        getinv <- function() inv 
        
        # Create special list
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv
             ) 
}


## cacheSolve takes special 'matrix' object created by makeCacheMatrix and 
## returns its inverse. It first looks to see if the inverse is allready stored
## in the cache. If it is, it returns that value and doesn't do any further 
## computation. If not, it uses the solve function to calculate the inverse
## store it in cache and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() #return inv value
        
        # Check this value to see if its is not Null
        # If it is not Null, return that value and exit the function
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) #return the value of inv and exit function
        }
        
        # Otherwise, we need to create the inverse
        # First go and get the matrix
        data <- x$get() 
        
        #now use solve to create its inverse
        inv <- solve(data) 
        
        #now use this value to set the cached inverse
        x$setinv(inv) #pass the inverse to the setinv to cache result
        
        inv #return the inverse
}

