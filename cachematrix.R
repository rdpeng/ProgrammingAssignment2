## Set of Functions that create a matrix and calculate its inverse (solve)
## when run if the inverse has already been solved, returns the cached value to save processing time

## creates the "matrix" object that can cache its inverse (solve) 

makeCacheMatrix <- function(x = matrix()) { 
        s <- NULL                               ## set initial value of S matrix to NULL ready 
        ## to take inverse values (solve)
        
        set <- function(y) {                    ## set function to reset initial values
                x <<- y
                s <<- NULL
        }
        get <- function() x                     ## get function to return value of x 
        setsolve <- function(solve) s <<- solve ## setsolve function solves (inverse) values of x   
        getsolve <- function() s                ## getsolve function gets and returns value of s
        list(set = set, get = get,              ## sets up functions for calling from cacheSolve
             setsolve = setsolve,
             getsolve = getsolve)
}


## calculates the inverse of the "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the original matrix has not changed), then the cacheSolve
## retrieves the inverse from the stored cache

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        
        if(!is.null(s)) {                       ## If s holds values ( not NULL ) retrieves cached 
                message("getting cached data")  ## data and prints message plus data
                return(s)
        }
        data <- x$get()                         ## Else get data from x, store in variable 'data'
        s <- solve(data, ...)                   ## then solve the value of the variable 'data' 
        x$setsolve(s)                           ## and store in s
        
        s                                       ## Returns a matrix 's' that is the solved inverse of 'x'
        
}