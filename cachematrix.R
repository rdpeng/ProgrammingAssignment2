### Assignment: Caching the Inverse of a Matrix
### git módosítás

## >> makeMatrix and cacheSolve will efficiently calculate the inverse of a matrix
## >> from the second call of 'cacheSolve' it won't calculate the inverse but use the cached result

## >> makeMatrix needs a matrix as the input
## >> it set's 'm' object to NULL so this erases the previous calculation
## >> than prepares the environment for the next inverse calculation

makeMatrix <- function(x = matrix()) {
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

## >> cacheSolve needs and object created by makeMatrix function
## >> if 'm' object is not NULL, so it has been already calculated 
## >> than it returns the cached results instead of calculating again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getsolve()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setsolve(m)
            m
}


## >> here is how it works in practice
    x <- matrix(round(runif(25)*100,0),5,5)
    myMatrix <- makeMatrix(x)
    cacheSolve(myMatrix)
    cacheSolve(myMatrix) #this function won't calculate but use the cached result
