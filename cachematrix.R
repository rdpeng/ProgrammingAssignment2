## The following functions allows to create a matrix and calculate its inverse. 

## This function creates a list containing functions that: a) sets a matrix, b) gets the matrix values
## c) sets the inverse of the matrix and d) gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {     ## The function is initialized with an empty matrix
        m <- NULL                               ## Creates a m variable and sets to NULL
        set <- function(y) {                    ## This subfunction receives a matrix 
                x <<- y                         ## Super assigns x and m in the parent enviroment
                m <<- NULL
        }
        get <- function() x                     ## This subfunction grabs whatever matrix passed on to x
        setinverse <- function(inv) m <<- inv   ## The rest of the subfunctions allows to set a man and recall it.
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This functions receives the matrix created with makeCacheMatrix and checks if the inverse was already calculated,
## if not then it calculates it and saves it in cache. 
cacheSolve cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()                     ## Receives and inverse matrix from getinverse
        if(!is.null(m)) {                       ## Checks if there is something saved in m, if it does the it prints m.
                message("getting cached data")
                return(m)
        }
        data <- x$get()                         ## If not it receives the matrix from get
        m <- solve(data, ...)                   ##Calculates the inverse matrix
        x$setinverse(m)                         ## saves the calculated inverse matrix in the setinverse (cache)
        m                                       ## It prints the inverse matrix
}

