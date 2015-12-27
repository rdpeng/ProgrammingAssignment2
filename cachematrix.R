## This program will use a combination of two functions to 
## compute the inverse of a matrix, and cache the resulting 
## inverse matrix.  If the inverse of the matrix has already 
## been calculated, the inverse will not be calculated, and
## the cached inverse matrix will be returned.  This program
## assumes all provided matricies are invertable.

## Since matrix inversion is a costly computation, this will
## provide a benefit when a matrix would otherwise be
## computed repeatedly.



## This function takes a matrix as an input and returns a 
## list that:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse matrix
## 4. gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv,
             getinv = getinv)
}



## This function will check to see if there is a cached
## inverse that has already been calculated.  If there
## is, it will return the cached inverse matrix, otherwise
## the inverse matrix will be calculated and returned.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
        message("getting cached data")
        return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
