## These 2 functions cooperate to get the invert of a matrix but without
## computing the result more than once. They do so by storing the result in the
## cache when computed.
## 

## This function create an object holding the variable and the value of the
## cached result (here a matrix and its invert) and giving getters and setters for
## these 2 objects

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL   #set the inverted matrix to NULL before all
    set <- function(y) { #when the matrix to invert is set or changed
        x <<- y     #set the (new) matrix
        i <<- NULL #set the inverted matrix to NULL (you cannot have calculated it yet)
    }
    get <- function() x #just return the value
    setinvert <- function(invert) i <<- invert #cache the value of calculated invert
    getinvert <- function() i #just return the value
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert) #return the list of functions (like some sort of encapsulation maybe)
}


## This function check if the result is in the cache (ie has been already
## calculated) and then return it, or else calculate it, store it in the cache
## and finally return it.

cacheSolve <- function(x, ...) {
    i <- x$getinvert() #first get the value from the cache
    if(!is.null(i)) { #if not null return it
        message("getting cached data")
        return(i)
    }
    data <- x$get() #else get the matrix to invert
    i <- solve(data, ...) #solve it
    x$setinvert(i) #cache the result
    i #return the result at last
}

#Sorry for any typo, English is not my native language