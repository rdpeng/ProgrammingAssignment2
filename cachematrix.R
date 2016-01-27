## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <-NULL                    #set initial vale of m as NULL
    set <- function(y){         #set the value of the matrix
        x <<- y                 #we use the signal `<<-` to assign values to 
        m <<- NULL              #objects x and m in an environment that is different
    }                           #from the current environment               
    get <- function() x                        #get the value of the matrix
    setinverse <- function(solve) m <<- solve  #set the value of the inverse
    getinverse <- function() m                 #get the value of the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)              #return the list containing the
                                               #value we need
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){                  #If the inverse has already been calculated
        message("getting cached data")#then the cachesolve should retrieve the inverse from the cache
        return(m)                      
    }
    data <- x$get()                  #otherwise, the codes should be run to compute          
    m <- solve(data,...)             #the inverse of the matrix by itself  
    x$setinverse(m)
    m
}