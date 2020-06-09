makeCacheMatrix <- function (x=matrix()){ ##function for special matrix 
        k <- NULL ##index
        set <- function (y){
                x <<- y 
                k <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse)k <<- inverse
        getInverse <- function()k
        list(set=set, get=get, setInverse = setInverse,getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        k <- x$getInverse() 
        
        if (!is.null(k)){
                message("getting cached data")
                return(k)
        }
        mat <- x$get() ##get matrix 'x'
        k <- solve(mat,...)
        x$setInverse(k)
        k
}

##Defining a regular matrix
A <- matrix(c(2,1,1,3,2,1,2,1,2), nrow=3, ncol=3)
A1 <- makeCacheMatrix(A)

##Getting matrix inverse
cacheSolve(A1)

