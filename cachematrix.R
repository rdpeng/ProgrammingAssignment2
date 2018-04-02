## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. For the excersise , we always assume
## that the matrix CAN be inversed

makeCacheMatrix <- function(x = matrix()) {     ## starting the function for the matrix caching. Setting x as a default empty matrix
        m <- NULL                               ## using similar procedure as the GetVector example, thus creating a Null matrix object
        set <- function(y) {                    ## set function for assigning values in the parent enviroment
                x <<- y                         ## x is superassigned in the parent enviroment (<<-)
                m <<- NULL                      ## m is superassigned as Null, to ensure that it clears any value it had before the 
        }                                       ## makeCacheMatrix function execution
   get <- function() x                          ## create get function, without placing x as argument (thus making the function
                                                ## retrieve it from parent enviroment)
   setinverse <- function(inverse) m <<- inverse   
                                                ##superassigns the value of m in parent environment via setinverse function
   getinverse <- function() m                   ## gets the value m when prompted to, via getinverse function
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                                                ## gives the appropriate names to the functions: set,get,setinverse,getinverse so that
}                                               ## I can call them with $ by name.

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()                            
        if(!is.null(m)) {                        ## checks if m is NOT Null (thus already calculated), and if it is not Null,
                message("getting cached data")   ## it gets it from the cache, alerting with a message
                return(m)                        ## returns it to the parent enviroment
        }
        data <- x$get()                          ## if m was Null, it retrieves the matrix from the input object
        m <- solve(data, ...)                    ## it calculates the matrix inversion via the solve function
        x$setinverse(m)                          ## sets the inverse matrix value to the input object
        m                                        ## prints the ivnerted matrix
} 
