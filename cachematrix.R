## These functions will find the inverse of a matrix and save it

## Write a short comment describing this function

makeCacheMatrix <- function(M = matrix()){
    inv <- NULL
    
    set <- function(mat){
        M <<- mat
        inv <<- NULL
    }
    get <- function() M
    
    set.inv <- function(inverse) inv <<- inverse
    get.inv <- function() inv
    
    list(set = set, get = get, set.inv = set.inv, get.inv = get.inv) 
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <- x$get.inv()
    
    if(!is.null(inv)){
        print("getting cached inverse")
        return(inv)
    }
    
    matrix <- x$get()
    inverse <- solve(matrix)
    x$set.inv(inverse)
    return(inverse)
        
}