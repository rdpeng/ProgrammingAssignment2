## makeCacheMatrix provides a list of functions for storing/retrieving/setting/caching a matrix
##cacheSolve calculates the inverse of the matrix and returns cached matrix
##further annotations below

#create a list of functions that cacheSolve can utilize
makeCacheMatrix <- function(x = matrix()) { 
    m<-NULL #create the NULL variable "m" for this environment
    set<-function(y){ #set/change the matrix and clear the cache.
        x<<-y
        m<<-NULL
    }
    get<-function() x #return the matrix "x"
    setInverse<-function(inv) m<<-inv #cache inv as "m"
    getInverse<-function() m #return m from cache
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) #create the list of functions
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
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
