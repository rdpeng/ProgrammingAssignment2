## Matrix inversion is usually a costly computation. 
## The following two functions allow us to cache the inverse of a matrix.

## The first function creates a special "matrix" object that can cache its inverse.
## It is actually a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function() i
        list(set = set,get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The second function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <-x$get()
        i<- solve(data,...)
        x$setinverse(i)
        i
}
