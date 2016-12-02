
## The first function, makeCacheMatrix, creates a special "vector", 
## which is really a list containing a function to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## set the value of the vector
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
                }
        ## get the value of the vector
        get = function() x
        ## set the value of the inverse
        setinv = function(inverse) inv <<- inverse
        ## get the value of the inverse
        getinv = function() inv
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)

}

## The following function, cacheSolove, calculates the inverse of the special "vector" 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        ## check to see if the inverse has already been calculated
        if(!is.null(inv)) {
                ## if so, get the inverse from the cache
                message("getting cashed data")
                return(inv)
                }
        ## otherwise, calculate the inverse of the data
        data = x$get()
        inv = solve(data, ...)
        ## set the value of the inverse in the cache
        x$setinv(inv)
        return(inv)
        
}
                
