## creates the vector x as a list of functions

makeCacheMatrix  <- function(x = numeric()) { # defines the funktion "makeCacheMatrix " with the numeric argument x 
        m <- NULL # initializes the variable m
        set <- function(y) {  # defines the funktion "set" with the argument y. Set is a subfunktion of "makeCacheMatrix "
                x <<- y # associates the argument y of the function "set" with the argument x of the funktion "makeCacheMatrix " in the parent environment
                m <<- NULL # initializes the variable m in the parent environment
        }
        get <- function() x # defines the function "get" without arguments. x is returned as matrix
        setinverse <- function(inverse) m <<- inverse # defines the function "setinverse", which contains the variable "inverse" as an argument, which is written into m in the parent environment
        getinverse <- function() m # analoguous to the function "get". m is passed to the function as an object which contains the variable "inverse"
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)# generates the special vector x, i.e. a list of functions
}


## calculates inverse of matrix if it is not stored in cache

cacheSolve <- function(x, ...) { # defines the function "cacheSolve" with the argument x and ...  as placeholder for additional arguments
        m <- x$getinverse() # the function "getinverse", being a part of the vector x, is written into the object m
        if(!is.null(m)) { # if m (the cache for the inverse matrix) is not empty, m and the message are returned
                message("getting cached data")
                return(m)
        }
        data <- x$get() # if m is empty, the matrix is written into the object "data", bei applying the "get" function from the list x to it
        m <- solve(data, ...) # the matrix "data" is inverted and written into m
        x$setinverse(m) # the function "setinverse" from the list x is applied to m 
        m # m is returned
}


