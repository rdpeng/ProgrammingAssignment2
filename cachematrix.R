## work for the week 3 assignment
## 
## takes a matric then creates an object that can store the matrix invers.
#also returns a list of the functions for the object.

makeCacheMatrix <- function(x = matrix()) {
#
        inverse <- NULL
        #
        set <- function(y) {
                            x <<- y
                            inverse <<- NULL
                           }
        
        get <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        
        #various functions within
        list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
        
}


## will provide the inverse of the matrix from the makeCascheMatrix if it already
# exists or if not just calculate it anew.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        #case of prior existence
        if (!is.null(inverse)) {return(inverse)}
        
        data <- x$get()
        #print(data) #checking original matrix
        
        #calculate
        inverse <- solve(data, ...)
        
        #cache
        x$setInverse(inverse)
        
        #give the inverse
        inverse        
}
