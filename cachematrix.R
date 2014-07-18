## These 2 functions enable caching of a matrix inverse.

## Creates special "matrix" object that can cache its inverse. Object is a list
## containing functions to set matrix, get matrix, set inverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y) {
                x<<-y
                inverse<<-NULL
        }
        
        get<-function() x
        set_inverse<-function(inverse_A) inverse<-inverse_A
        get_inverse<-function() inverse
        
        list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


## Calculates the inverse of matrix. It first checks if the inverse has already
## been calculated. If so it gets cached value and skips computation. Otherwise,
## it calculates the inverse and store it in the cache.

cacheSolve <- function(x, ...) {
        inverse<-x$get_inverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data<-x$get()
        inverse<-solve(data, ...)
        x$set_inverse(inverse)
        inverse
}
