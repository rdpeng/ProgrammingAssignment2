
##Catching the inverse of a Matrix.
## This function creates a speacil "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
 set <-function (y) {
         x<<-y
         inv<<-NULL
 }
 get<-function()x
 setInverse<-function(inverse) inv<<-inverse
 getInverse<-function() inv
 list(set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)
}


## The following function calculates the mean of the special "vector" created with the above function. 

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
