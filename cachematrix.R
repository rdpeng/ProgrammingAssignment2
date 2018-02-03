## Put comments here that give an overall description of what your
## functions do

# Make a special matrix that can cache its inverse


makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL  #placeholder for our matrix
        
        set <- function(y) {            #set the matrix passed into makeCacheMatrix to the value passed in here
                mat <<- y               # That way we have the ability to permanently assign our arugments
                inv <<- NULL            
        }
        
        get <- function() mat  #return the matrix that we 'set'
        setInverse <- function(inverse) inv <<- inverse  #if we have computed an inverse, toss it to setInverse to apply it to inv
        getInverse <- function() inv
        
        # Return all of this beautiful logic in a list of functions.
        # These functions can then be used by any variable that is assigned to the makeCacheMatrix() funciton
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



# The following function calculates the inverse of the special "matrix" created 
# with the above function. However, it first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the data 
# and sets the value of the inverse in the cache via the setInverse() function.


cacheSolve <- function(mat, ...) {
        inv <- mat$getInverse()  #Attempt cache retrieval
        if (!is.null(inv)){
                message("Getting cached inverted matrix")
                return(inv)
        }
        
        # If we didnt find one, we need to make one
        data <- mat$get()     
        if (!is.matrix(data)){
                return("Data is not a matrix!")
        }
        inv <- solve(data,...)   #R's solve function will invert a matrix
        mat$setInverse(inv)  # We have our inverse! Now cache...
        inv   #... and return!
}

