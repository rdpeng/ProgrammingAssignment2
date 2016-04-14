## makeCacheMatrix provides a list of functions for storing/retrieving/setting/caching a matrix
##cacheSolve calculates the inverse of the matrix and utilizes makeCacheMatrix functions to cache the new matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL #create the NULL variable "m" for this environment
    set<-function(y){ #change the matrix to y without re-calling makeCacheMatrix, or allows cacheSolve to set the matrix because x has a default case. Clears the cache
        x<<-y
        m<<-NULL
    }
    get<-function() x #return the matrix "x"
    setInverse<-function(inv) m<<-inv #cache inv as "m"
    getInverse<-function() m #return m from cache
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) #create the list
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse() #store cached inverse as "m", could be NULL
    if(!is.null(m)) { #if m is not null, return the cached value
        message("getting cached data")
        return(m)
    }
    data <- x$get() #store the matrix as "data"
    m <- solve(data, ...) #calculate inverse of the matrix
    x$setInverse(m) #caches the inverse
    m
}
