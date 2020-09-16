## This function creates a special "matrix" object that can 
## cache its inverse.

## Which is really a list containing a function to
## 1) set the values of the matrix
## 2) get the values of the matrix
## 3) set the values of the inverse matrix
## 4) get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set<- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse){
                inv <<- inverse
        }
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }else{
                Data<- x$get()
                inv<- solve(Data)
                x$setinv(inv)
                inv
        }
}
