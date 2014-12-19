## Below is a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

## makeCacheMatrix creates a special object, that stores the matrix user is looking forward to inverse, 
## and an inversed matrix, when the inversion has been done. The object has three functions:
##  -- get() - returns the original matrix
##  -- setInverse() - stores the inversed matrix
##  -- getInverse() - returns inversed matrix if was previously stored or NULL

makeCacheMatrix <- function(x = matrix()) { #input x will be a matrix
        
        i <- NULL # placeholder to store inversed matrix; resets to NULL everytime the function is called
        
        get <- function() x # returns the original matrix
        
        setInverse <- function(inverseMatrix) { # stores the inversed matrix
                i <<- inverseMatrix
        }
        
        getInverse <- function() i # returns matrix that is alredy cached
        
        list(get = get, setInverse = setInverse, getInverse = getInverse) # returns an object with correspondig methods        
}

## cashSolve gets object, created by makeCacheMatrix, checks whether the inversed matrix has been already stored. 
## If yes, it returns stored inversed matrix, if no, it calculates the inversed matrix, stores it to the object and 
## returns it.

cacheSolve <- function(x, ...) { # x is an object created by makeCacheMatrix
        
        i <- x$getInverse() # accesses cached matrix
        
        if(!is.null(i)) {
                message("getting cached data") # indicates that cached data was used
                return(i) # casheSolve returns cached data
        }
        
        # code below get accessed if no cached data has been previously stored
        
        matrix <- x$get() # gets matrix from an argument
        i <- solve(matrix) # inverses this matrix
        x$setInverse(i) # stores inversed matrix into cache
        i # function returns inversed matrix
}