## Assingnment 2, write functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        get<-function() x
        set<-function(y){
                x<<-y
                inv<<-NULL
}
        getinv<-function() inv
        setinv<-function(inverse) inv<<--inverse
        
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        inv<-x$getinv()
        if(!is.null(inv)) {
                message("inverse is cached")
                return(inv)
        }
        m<-x$get()
        inv<-solve(m, ...)
        x$setinv(inv)
        inv
}
