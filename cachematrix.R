## Function calculates and stores the inverse of a given matrix for easy retrieval
## in case it needs to be accessed again at a later time. This makes use of the lexical scoping 
## feature of the R programming language and is especially helpful when working with
## large datasets and computationally intensive calculations

## First function takes as input a matrix and returns a list containing 4 functions (2 get and 2 set functions).
## In addition, it contains "m" and "x" due to the lexical scoping feature.

makeCacheMatrix <- function(x = matrix()) {
        # z (inverse of x) is originally set to NULL
        z <- NULL
        
        # set function can change the input matrix x and RESETS the previously calculated inverse
        set <- function(y){
                x <<- y
                z <<- NULL
        }
        
        # get function retrieves the currently stored input matrix x
        get <- function() x
        
        # setinverse function takes an input and stores it as the inverse z
        setinverse <- function(inverse) z <<- inverse
        
        # getinverse retrieves the currently stored inverse matrix z of current input matrix x
        getinverse <- function() z
        
        # return a list containing above functions
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Second function takes as input an object returned in the first function and does one of two things:
## It calculates the inverse of the matrix if it is the first time this is called
## Or in case the inverse is already stored in cache it retrieves it
## In any case the function returns an inverse matrix of the matrix originally stored in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. input x in function is an output list returned in
        ## previous function

        # get the inverse z stored in the cache object
        z <- x$getinverse()
        
        # Determine if the inverse has previously been calculated (Always reset to NULL on new input)
        if (!is.null(z)){
                message("getting cached data")
                return(z)
        }
        
        # get the currently stored input matrix from the cache object
        data <- x$get()
        
        # actual calculation of the inverse of input matrix
        z <- solve(data)
        
        # store the calculated inverse output matrix z back in the cache object using the setinverse function
        x$setinverse(z)
        
        # return the output inverse matrix to the console
        z
}
