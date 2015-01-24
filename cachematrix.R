### These functions will take advantage of caching functionality available in R
### to save time and computing resources needed for inversion of matrices.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL  #sets the value of m to NULL (a default in case cacheSolve has not yet been used)
        set<-function(y){  #sets the value of the matrix
                x<<-y  #caches the inputted matrix so that cacheSolve can check whether it has changed
                m<<-NULL  #sets the value of m (the matrix inverse if used cacheSolve) to NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,  #creates a list to store the four functions
             setinverse = setinverse,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()  # if an inverse has already been calculated, get it
        if(!is.null(m)){  #checks if cacheSolve has been run before
                message("getting cached data")  #sends a text message
                return(m)  #returns the cached matrix
        }  #otherwise
        matrix<-x$get()  #gets the value of the input matrix
        m<-solve(matrix, ...)  #computes the value of the inverse of the input matrix
        x$setmatrix(m)  #caches the inverse
        m  #returns the inverse
}
