makeCacheMatrix <- function(x = matrix()) {

        local_m <- NULL                                         ## Initialize the local m to NULL   

        set <- function(y) {                                    ## Create set function to store the matrix passed in the call as x and NULL as m, both in cache.

                cache_x <<- y                                   

                cache_m <<- NULL                                ## Initialize caache_m to NULL           

        }

        get <- function() cache_x                               ## Create function to get/return the matrix passed in the command line call to '$set

        set_cache_m <- function(local_m) cache_m <<- local_m    ## Create function to set the value of cache_m in cache to the value of local_m passed in the call to '$set_cache_m.        

        get_cache_m <- function() cache_m                       ## Create function to retrieve value of cache_m from cache and return cache_m to the caller so we can check it for NULL

        list(set = set, get = get,

             set_cache_m = set_cache_m,

             get_cache_m = get_cache_m)

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## cacheSolve function receives a variable that is a matrix that is expected to have been defined as makeCacheMatrix(),
## as in m <- makeCacheMatrix(), and then populated with an invertible matrix using the m$set() function that is nested 
# #in makeCacheMatrix()


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x) {                     
        ## Receive makeCacheMatrix from the caller.

        local_m<- x$get_cache_m()               
        if(!is.null(local_m)) {                 
                ## Check to see if m is NULL.  

                message("getting cached data")  
                ## If m is not NULL, return the value of m with a message.

                return(local_m)

        }                                      
        startingmatrix <- x$get()               ## Call the nested function x$get in makeCacheMatrix to obtain the UNinverted matrix with which to start, and assign it to startingmatrix.                         

        endingmatrix <- solve(startingmatrix)   ## Use solve() to invert the startingmatrix.  Assign the result to endingmatrix.

        x$set_cache_m(endingmatrix)             ## Call nested function x$set_cache_m() in makeCacheMatrix to set m in the cache environment to the local non-NULL inverted result in endingmatrix

        endingmatrix             
}
}
