## This function creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) { ## computes the matrix
    inv <- NULL ## will hold the matrix inverse value, initalizes as NULL
    set <- function(y) { ## define the set where new value assigned
        x <<- y ## value of matrix in parent environment
        inv <<- NULL ## if there's a new matrix reset inverse to NULL
    }
    get <- function() x # will be assigned the value after setting and getting inverse
    setinverse <- function(inverse) inv <<- inverse # sets the value of the matrix
    getinverse <- function() inv # gets the value of the matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
        # helper list so we can use $ operator to identify column names later
}


# The following function calculates the inverse of the special "matrix" created with the 
# above function. However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation. Otherwise, it 
# calculates the inverse of x and sets the value of the inverse in the cache via the 
# setinverse function.


cacheSolve <- function(x, ...) { ## Returns a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cleared data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

