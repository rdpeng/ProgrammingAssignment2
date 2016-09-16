# This program is to compute the inverse of input matrix and retrieve the result from the cache
# There are two variables in this program
# x is the matrix that needs to be processed
# i stores the inverse of matrix

makeCacheMatrix <- function(x = matrix()){
    
    i <- NULL
    
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inv) i<<-inv
    
    getinv <- function() i
    
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

cacheSolve <- function(x, ...){
    #the input x must be a makeCaheMatrix
    
    i <- x$getinv()
    
    #check whether i has already been calculated or not
    #if so, return the cached result and skip the calculation
    
    if(!is.null(i)){
        message("Getting the cached data...")
        return(i)
    }
    
    #if i needs to be calculated, calculate i as follow
    
    dataMatrix <- x$get()
    i <- solve(dataMatrix, ...)
    
    #set the inverse matrix in the cache
    x$setinv(i)
    
    #return the result
    return(i)
    
}