## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    cache <- NULL
    
    set <- function(y) {
        x<<-y
        cache<<- NULL
    }
    
    get <- function() x
    
    set_cache <- function(inverse) cache <<- inverse
    
    get_cache <- function() cache
    
    
    
    list(set=set, get=get, set_cache = set_cache, get_cache=get_cache)
    
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    
    cache <- cache_store_var$get_cache()
    
    if (is.null(cache)) {
        #create cache
        mat <- cache_store_var$get()
        cache <- solve(mat)
        
        #store cache
        cache_store_var$set_cache(cache)
        return(cache)
        
    } else {
        
        # take stored cache
        message("getting cached data")
        return(cache)
    }
    
    
}
