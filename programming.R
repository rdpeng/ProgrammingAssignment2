## Unlike the usual single arrow assignment that always works on the current level, 
## the double arrow operator can modify variable in parent levels.
## Each time makeVector is run, it creates an environment.




### Assignment: caching the inverse of a matrix
makeCacheMatrix <- function(x = matrix()){
        Inverse <- NULL
        set <- function(y){
                x <<- y
                Inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) Inverse <<- inverse
        getinverse <- function() Inverse
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse= getinverse)
        
}



cacheSolve <- function(x, ...){
        Inverse <- x$getinverse()
        if(!is.null(Inverse)){
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setinverse(Inverse)
        Inverse
        
}






