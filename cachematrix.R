## Computation of a matrix inverse is very often a time consuming task. The
## following two functions "structure" an invertible matrix with get, set, 
## getInverse and setInverse members, so that its inverse can be later recalled
## from memory, and at ease by calling cacheSolve on the variable created through
## makeCacheMatrix.

## The following function creates a list containing a function to
## 1.set the value of the numeric matrix
## 2.get the value of the numeric matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
## Throw an error if the input is not numeric
    if(!is.numeric(x)) stop("not a numeric matrix")     
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the mean of the list created by the  
## above function. However, it first checks to see if the inverse has already 
## been calculated/attempted. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the data
## (if exists) and sets the value of the inverse in the cache via the 
## setInverse function (a 1-character array message if it does not exist).

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ## Check if matrix is square and of full rank and store an error msg if not
    if(dim(data)[1]!=dim(data)[2]||det(data)==0) {
        m<-("Not an invertible matrix")
        x$setInverse(m)
        return(m)
    }
    ## Return a matrix that is the inverse of 'x'
    m <- solve(data,diag(dim(data)[1]))
    x$setInverse(m)
    m
}
        
