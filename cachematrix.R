## Caching the inverse of a matrix
## Created Function will store a matrix and caches its inverse
## Essentially function will create a matrix object to cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
                }
        get<-function()x
        setInverse <- function(inverse) i<<- inverse
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
        

}


## This function computes the inverse of the "matrix" which is created from makecacheMatrix. Once the inverse is already calculated ( also no changes in matrix), 
## then we can retrive the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setInverse(i)
        i
}
