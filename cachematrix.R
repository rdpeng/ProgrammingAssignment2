## Function 1 will create an inverse matrix of the data
## Function 2 will retrieve the cached data

## Initialise inverse as NULL : invm <- NULL
## Get value of matrix : get <- function() x
## Set value of matrix : set <- function(y)
## Double arrow assignment encloses within a function
## Set inverse of matrix :
##      setInverse <- function(inverse) invm <<- inverse
## Get inverse of matrix : getInverse <- function() invm

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        } 
        get <- function() x
        setInverse <- function(inverse) invm <<- inverse
        getInverse <- function() invm
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Check if 'invm' is null, if 'invm' is not null... : if(!is.null(invm))
## Then return 'invm', which is the inverse matrix : return(invm)
## Get matrix, set to vector : matx <- x$get()
## 'Solve'/ calculate the inverse value : invm <- solve(matx, ...)
## Display 'invm'

cacheSolve <- function(x, ...) {
        invm <- x$getInverse()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        matx <- x$get()
        invm <- solve(matx, ...)
        invm
}
