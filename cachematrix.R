# Programming Assignment 2: Caching the Inverse of a Matrix
## My functions are useful when we want to compute an inversion of 
## matrix repeatedly. 

### The first function change matrix from user to a useful list of functions
### which is used in second function. 
### I think that the most important part is the cache where we can store 
### the inversion for later calculations.
### Everything is explain (in detail) in the function itself.

makeCacheMatrix <- function(x = matrix()) {        # takes an argument (class = matrix) from user
        m <- NULL                                  # setting cache for this function
        set <- function(y) {                       # setting first subfunction
                x <<- y                            # it takes the matrix and assigns it a variable named x; super assignment operator means that we change variable x in the parent environment
                m <<- NULL                         # this clears cache m; super assignment operator means that we change variable m in the parent environment
        }
        get <- function() x                        # setting second subfunction; returns matrix itself
        setinverse <- function(solve) m <<- solve  # setting third subfunction; assigns inversion (=input) to object m
        getinverse <- function() m                 # setting fourth subfunction; returns matrix inversion = returns cache
        list(set = set, get = get,                 # creates a list of four subfunctions, which will be returned by makeCacheMatrix
             setinverse = setinverse, 
             getinverse = getinverse)
}

### This function computes inversion matrix and saves it into a cache - so if we
### want to compute it again we take it from cache and we don't have to 
### compute it again.

cacheSolve <- function(x, ...) {               # funcion calculates inversion of matrix
        m <- x$getinverse()                    # returns cache into m (local variable of this function)
        if(!is.null(m)) {                      # it checks if the returned cache has anything in it (when we called it for the first time, it's FALSE)
                message("getting cached data") # if m isn't empty, it returns this message
                return(m)                      # and m (=returns cache)
        }
        data <- x$get()                        # saves matrix into local variable data
        m <- solve(data, ...)                  # puts matrix inversion into local variable m
        x$setinverse(m)                        # stores inversion into cache
        m                                      # returns inversion for user who called the funcion
}
