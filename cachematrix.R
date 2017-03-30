## 
## This function creates a special "matrix" object that can cache its inverse.


##  Set and get the value of the Matrix
        

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## The following function calculates the mean of the special "vector" created with the above function. 

cacheSolve <- function(x) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data)
        x$setsolve(m)
        m
}     ## Return a matrix that is the inverse of 'x'

